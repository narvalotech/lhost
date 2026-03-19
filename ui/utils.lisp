;; eval host.lisp first

(defun make-a-sign (byte)
  (if (logbitp 7 byte)
      (- byte (ash 1 8))
      byte))

(defun decode-scan-report (evt)
  ;; TODO: handle multiple reports
  (let ((report (nth 0 (getf (nth 1 evt) :reports))))
    (list :address-type (getf report :address-type)
          :address (decode-c-int (getf report :address))
          :timestamp (get-internal-real-time)
          :rssi (make-a-sign (getf report :rssi))
          :data (getf report :data))))

(defun merge-plist (a b)
  (loop for (key val) on b by #'cddr
        do (setf (getf a key) val))
  a)

(defun merge-reports (dest decoded)
  (let ((parsed (parse-ad (getf decoded :data)))
        (existing (getf (gethash (getf decoded :address) dest) :parsed)))
    (setf (getf (gethash (getf decoded :address) dest) :parsed)
          (if existing
              (merge-plist existing parsed)
              parsed))))

(defun accumulate-scan-reports (dest report)
  ;; Track the last 20 reports for each address
  (let ((decoded (decode-scan-report report)))
    (push decoded (getf (gethash (getf decoded :address) dest) :reports))
    (nbutlast (getf (gethash (getf decoded :address) dest) :reports) 20)

    ;; Parse and merge reports
    (merge-reports dest decoded)
    nil))

(defun extract-name (parsed)
  (let* ((encoded-name (or (getf parsed :name-complete)
                           (getf parsed :name-short))))
    (ignore-errors
     (if encoded-name
         (from-c-string encoded-name)
         ""))))

(defun make-device-list (devices-dict)
  (let ((devices))
    (maphash (lambda (address data)
               (declare (ignore address))
               (let* ((report (car (getf data :reports)))
                      (name (extract-name (getf data :parsed))))
                 (push (append report (list :name name)) devices)))
             devices-dict)
    devices))

(defun print-addr (address)
  (format nil "~{~2,'0X~^:~}"
          (subseq (make-c-int :u64 address t) 2)))

(print-addr #xF89DC52A5C01)
 ; => "F8:9D:C5:2A:5C:01"

(defun decode-mac (address-string)
  (ignore-errors
   (parse-integer (remove #\: address-string) :radix 16)))

;; look ma! end-to-end testing!
(print-addr (decode-mac (print-addr #xF89DC52A5C01)))
 ; => "F8:9D:C5:2A:5C:01"

(defun queue (mailbox event)
  (when event
    (sb-concurrency:send-message mailbox event)))

(defun sort-scan (by devices)
  (sort devices
        (case by
          (:recent (lambda (a b) (< (getf a :timestamp) (getf b :timestamp))))
          (:rssi (lambda (a b) (> (getf a :rssi) (getf b :rssi))))
          (:name (lambda (a b) (string-lessp (getf a :name) (getf b :name))))
          (otherwise (lambda (a b) (string-lessp (getf a :name) (getf b :name)))))))

(defun filter-scan (name-filter devices)
  (if (zerop (length name-filter))
      devices
      (remove-if-not (lambda (device)
                       ;; Case-insensitive search
                       (search name-filter (getf device :name) :test #'equalp))
                     devices)))

(defun make-connection-object (address-with-type handle treeview)
  (list :address-with-type address-with-type :handle handle :treeview treeview))

(defun handle->address (hci-handle connections)
  (maphash (lambda (address obj)
             (when (= (getf obj :handle) hci-handle)
               (return-from handle->address address)))
           connections))

(defun abbrev-attribute-type (attribute)
  (case attribute
    (:service "Service")
    (:characteristic-declaration " |  Char declaration")
    (:characteristic-value " |    | Char value")
    (:characteristic-descriptor " |    | Char descriptor")
    (otherwise " -- ")))

(defun format-attribute-data (attribute)
  ;; TODO: make this human-readable for svc, decl, desc
  (px (getf attribute :data)))

(defparameter *cmds* (make-mailbox "ui backend -> host"))
(defparameter *evts* (make-mailbox "ui backend <- host"))

(defparameter *controller* (make-controller))

(defun do-rx-work (hci ui-events)
  (drain-rxq hci)
  (loop
    (let ((packet (receive-rxq hci)))
      (queue ui-events packet)
      (if packet
          (process-hci hci (copy-tree packet))
          (return-from do-rx-work nil)))))

(defun encode-address (address)
  (list
   :address
   (subseq (make-c-int :u64 (getf address :address)) 0 6)
   :type (getf address :type)))

(defun start-backend (ui-events)
  (setf *controller* (make-controller))

  ;; Empty events mailbox
  (loop until (sb-concurrency:mailbox-empty-p ui-events))

  (list
   (start-hci
    (getf *controller* :tx-mailbox)
    (getf *controller* :rx-mailbox)
    (getf *controller* :stop-signal))

   (bt:make-thread
    (lambda ()
      (let ((hci *controller*)
            (*active-conns*)
            (*bonds* (make-hash-table))
            (bonds-stash))
        (loop
          (let ((cmd (sb-concurrency:receive-message *cmds* :timeout .1)))
            (when cmd
              (log-inf "[T] sending to host: ~X" cmd)
              (case (car cmd)
                (:init
                 (log-inf "INIT CONTROLLER")
                 (init-controller hci))

                (:start-scan
                 (progn
                   (log-inf "START SCAN")
                   (hci-set-scan-param hci)
                   (hci-set-scan-enable hci t)))
                (:stop-scan
                 (progn
                   (log-inf "STOP SCAN")
                   (hci-set-scan-enable hci nil)))

                (:connect
                 (let ((address (nth 1 cmd)))
                   (log-inf "CONNECT ~X" address)
                   (hci-set-scan-enable hci nil)
                   (hci-create-connection hci (encode-address address))))
                (:disconnect
                 (let ((conn-handle (nth 1 cmd)))
                   (log-inf "DISCONNECT")
                   (hci-disconnect hci conn-handle)))

                (:clear-security
                 (let ((conn-handle (nth 1 cmd))
                       (peer-address (nth 2 cmd)))
                   ;; Clear SMP context
                   (setf (get-smp-context conn-handle) '())

                   ;; SMP also reads *active-conns* for addresses
                   (setf (getf (getf *active-conns* conn-handle) :our-address)
                         (make-address (getf hci :random-address) #x01))
                   (setf (getf (getf *active-conns* conn-handle) :address)
                         peer-address)))
                (:bond
                 (let ((conn-handle (nth 1 cmd)))
                   (log-inf "BOND")
                   (start-security hci conn-handle)))
                (:stash-bonds
                 (progn
                   (log-inf "STASH BONDS")
                   (setf bonds-stash *bonds*)
                   (setf *bonds* (make-hash-table))))
                (:unstash-bonds
                 (progn
                   (log-inf "UN-STASH BONDS")
                   (when bonds-stash
                     (setf *bonds* bonds-stash)
                     (setf bonds-stash nil))))

                (:discover-gatt
                 (let* ((conn-handle (nth 1 cmd))
                        (gattc-table (gattc-discover hci conn-handle)))
                   (queue ui-events (list :gatt-discovery-end
                                          conn-handle
                                          gattc-table))))
                (:att-read
                 (log-inf "ATT READ"))
                (:att-write
                 (log-inf "ATT WRITE"))
                (:att-notify
                 (let* ((conn-handle (nth 1 cmd))
                        (handle (nth 2 cmd))
                        (data (nth 3 cmd)))
                   (log-inf "ATT NOTIFY C ~X H ~X D ~X" conn-handle handle data)
                   (notify hci conn-handle handle data)))
                (:quit
                 (progn
                   (log-inf "Exiting UI host interface")
                   (return nil)))))
            (do-rx-work hci ui-events)
            ))))
    :name "Host interface")))

(defun stop-backend (backend-thread &optional send-quit-cmd)
  (ignore-errors
   (when backend-thread
     (when send-quit-cmd
       (queue *cmds* (list :quit)))
     (stop-hci (getf *controller* :stop-signal))
     (bt:destroy-thread (nth 0 backend-thread))
     (bt:destroy-thread (nth 1 backend-thread))
     (sleep .5)
     (log-inf "KILLED ALL THREADS"))
   (loop while (drain-mailbox *evts*))
   (loop while (drain-mailbox *cmds*))

   ;; And a brand-new controller
   (setf *controller* (make-controller))
   ))

(defun dispatch-cmd (cmd-id &rest args)
  (log-inf "DISPATCH ~A~A" cmd-id (if args (format nil " ARGS ~X" args) ""))
  (queue *cmds* (push cmd-id args))
  t)
