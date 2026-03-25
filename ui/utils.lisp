(require 'asdf)
(require :host)
(in-package :host)

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

(defparameter *cmds* (make-mailbox "ui backend -> host"))
(defparameter *evts* (make-mailbox "ui backend <- host"))

(defparameter *controller* (make-controller))

(defparameter *ui-events* nil)

;; SSL added and removed here </meme>
(defun add-to-rxq (hci packet)
  (when (cadr packet)
    (queue *ui-events* (copy-tree packet))
    (push packet (getf hci :rxq))))

(defun do-rx-work (hci)
  (drain-rxq hci)
  (loop
    (let ((packet (receive-rxq hci)))
      (if packet
          (ignore-errors
           (process-hci hci (copy-tree packet)))
          (return-from do-rx-work nil)))))

(defun encode-address (address)
  (list
   :address
   (subseq (make-c-int :u64 (getf address :address)) 0 6)
   :type (getf address :type)))

(defun save-hash-table (table filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    ;; Store the test type so we can recreate it correctly
    (print (hash-table-test table) out)
    ;; Store the data as an alist
    (let ((alist '()))
      (maphash (lambda (k v) (push (cons k v) alist)) table)
      (print alist out))))

(defun load-hash-table (filename)
  (with-open-file (in filename)
    (let* ((test (read in))
           (alist (read in))
           (table (make-hash-table :test test)))
      (dolist (pair alist)
        (setf (gethash (car pair) table) (cdr pair)))
      table)))

(defvar *packetizer-path* "COM6")
(defvar *latest-gattc-table* nil)

(defun start-backend (ui-events)
  (setf *controller* (make-controller))
  (hci-log-reset)

  ;; Empty events mailbox
  (loop until (sb-concurrency:mailbox-empty-p ui-events))

  (list
   (start-hci
    *packetizer-path*
    (getf *controller* :tx-mailbox)
    (getf *controller* :rx-mailbox)
    (getf *controller* :stop-signal))

   (bt:make-thread
    (lambda ()
      (let ((hci *controller*)
            (*active-conns*)
            (*ui-events* ui-events)
            (*bonds* (make-hash-table))
            (bonds-stash))
        (declare (special *ui-events*))
        (loop
          (let ((cmd (sb-concurrency:receive-message *cmds* :timeout .1)))
            (when cmd
              (log-inf "[T] sending to host: ~X" cmd)
              (case (car cmd)
                (:init
                 (log-inf "INIT CONTROLLER")
                 (init-controller hci)
                 (queue ui-events
                        (list :gatt-server-table
                              *gatts-table*)))

                (:start-adv
                 (progn
                   (start-advertising hci (list
                                           (make-ad :flags '(#x01)) ; LE General discoverable
                                           (make-ad :class-uuid-16-incomplete
                                                    (make-c-int :u16 +gatt-uuid-heart-rate-service+))
                                           (make-ad-name "HRM 600 (evil)")))
                   (log-inf "START ADV OK")))
                (:stop-adv
                 (hci-set-adv-enable nil hci))

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
                 (let ((conn-handle (nth 1 cmd))
                       (is-central (nth 2 cmd)))
                   (log-inf "BOND")
                   (start-security hci conn-handle :is-peripheral (not is-central))))
                (:notify-bond
                 (let ((conn-handle (nth 1 cmd)))
                   (queue ui-events
                          (list :bonded
                                (getf (getf *active-conns* conn-handle) :address)))))
                (:stash-bonds
                 (unless bonds-stash
                   (log-inf "STASH BONDS")
                   (setf bonds-stash *bonds*)
                   (setf *bonds* (make-hash-table))))
                (:unstash-bonds
                 (progn
                   (log-inf "UN-STASH BONDS")
                   (when bonds-stash
                     (setf *bonds* bonds-stash)
                     (setf bonds-stash nil))))
                (:load-bonds
                 (let ((bond-filename (nth 1 cmd)))
                   (log-inf "LOAD BONDS (path: ~A)" bond-filename)
                   (ignore-errors
                    (setf *bonds* (load-hash-table bond-filename))
                    (log-inf "load ok: ~X" (bonds->list *bonds*))
                    (maphash (lambda (k v)
                               (declare (ignore k))
                               (queue ui-events (list :bonded (getf v :peer))))
                             *bonds*))
                   ))
                (:store-bonds
                 (unless bonds-stash    ; do not overwrite real bonds with temp
                   (let ((bond-filename (nth 1 cmd)))
                     (log-inf "STORE BONDS (path: ~A)" bond-filename)
                     (ignore-errors
                      (save-hash-table *bonds* bond-filename)
                      (log-inf "store ok"))
                     )))
                (:clear-bonds
                 (progn
                   (log-inf "CLEAR BONDS")
                   (setf bonds-stash nil)
                   (setf *bonds* (make-hash-table))))

                (:discover-gatt
                 (let* ((conn-handle (nth 1 cmd))
                        (gattc-table
                          (handler-case
                              (sb-ext:with-timeout 5
                                (gattc-discover hci conn-handle))
                            (sb-ext:timeout ()
                              (log-err "GATT DISCOVERY TIMED OUT")
                              nil))))
                   (setf *latest-gattc-table* gattc-table)
                   (log-dbg "DISCOVERED: ~A"
                            (gattc-print gattc-table))
                   (when gattc-table
                     (queue ui-events (list :gatt-discovery-end
                                            conn-handle
                                            gattc-table)))))
                (:att-read
                 (let* ((conn-handle (nth 1 cmd))
                        (att-handle (nth 2 cmd))
                        (data (att-read hci conn-handle att-handle)))
                   (log-inf "ATT READ: ~X" data)
                   (queue ui-events (list :att-read-rsp
                                          conn-handle
                                          att-handle
                                          data))
                   ))
                (:att-write
                 (let ((conn-handle (nth 1 cmd))
                       (att-handle (nth 2 cmd))
                       (data (nth 3 cmd)))
                   (log-inf "ATT WRITE")
                   (att-write hci conn-handle att-handle data)))
                (:att-write-cmd
                 (let ((conn-handle (nth 1 cmd))
                       (att-handle (nth 2 cmd))
                       (data (nth 3 cmd)))
                   (log-inf "ATT WRITE CMD")
                   (att-write hci conn-handle att-handle data t)))
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
            (do-rx-work hci)
            ))))
    :name "Host interface")))

(defun stop-backend (backend-thread &optional send-quit-cmd)
  (hci-log-write)
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
