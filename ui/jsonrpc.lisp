;; eval utils.lisp before
(ql:quickload '(:jsonrpc :usocket :yason))
(setf yason:*list-encoder* 'yason:encode-alist)

(defun make-att-ribute-type (type)
  (case type
    (:service "service")
    (:characteristic-declaration "characteristic_declaration")
    (:characteristic-value "characteristic_value")
    (:characteristic-descriptor "characteristic_descriptor")))

(defun uuid128 (uuid)
  (if (= (get-uuid-size uuid) 16)
      uuid
      0))

(defun uuid16 (uuid)
  (if (= (get-uuid-size uuid) 2)
      uuid
      0))

(defun gatt->json (gatt-table)
  (list (cons "attributes"
              (coerce
               (loop for attribute in gatt-table
                     collect
                     (list
                      (cons "handle" (getf attribute :handle))
                      (cons "att_type" (make-att-ribute-type (getf attribute :type)))
                      (cons "uuid16" (uuid16 (getf attribute :uuid)))
                      (cons "uuid128" (uuid128 (getf attribute :uuid)))))
               'vector))))

(gatt->json *gatts-table*)

(defun make-server-discovered (table)
  (list (cons "server_discovered"
              (list
               (cons "conn_handle" #xFFFF)
               (cons "gatt" (gatt->json table))))))

(defun make-client-discovered (conn table)
  (list (cons "discovered"
              (list
               (cons "address"
                     (list (cons "address_type" 0)
                           (cons "address" 0)))
               (cons "conn_handle" conn)
               (cons "gatt" (gatt->json table))))))

(defun yap (input)
  (with-output-to-string (os)
    (yason:encode input os)))

(ql:quickload :bordeaux-threads)
(defparameter *server* (jsonrpc:make-server))
;; TODO: move to raw sockets
(bt:make-thread
 (lambda ()
   (jsonrpc:server-listen *server* :port 55000 :mode :tcp))
 :name "JRPC server")

(defun make-scan-result (device)
  (destructuring-bind
      (&key address-type address rssi data &allow-other-keys)
      (car (getf device :reports))
    (list (cons "address"
                (list (cons "address_type" address-type)
                      (cons "address" address)))
          (cons "rssi" rssi)
          (cons "name" (extract-name (getf device :parsed)))
          (cons "data" (coerce data 'vector))
          (cons "decoded" (format nil "~X" (getf device :parsed))))))

(defun make-scan-results (results)
  (let ((r (make-array 100 :fill-pointer 0 :adjustable t)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (vector-push-extend
                (make-scan-result v) r))
             results)
    (list (cons "scan_results"
                (list (cons "results" (coerce r 'vector)))))))

(defun make-conn-complete (address conn)
  (list (cons "conn_complete"
              (list (cons "conn_handle" conn)
                    (cons "address"
                          (list (cons "address_type" (car address))
                                (cons "address" (cadr address))))))))

(defun make-disconnected (handle)
  (list (cons "disconnected"
              (list (cons "conn_handle" handle)))))

(defun make-att-event (repr)
  (list (cons "att_packet"
              (list (cons "conn_handle" #xFFFF)
                    (cons "op" #xFF)
                    (cons "handle" #xFFFF)
                    (cons "data" #())
                    (cons "repr" repr)))))

(defparameter *cmds* (make-mailbox "ui backend -> host"))
(defparameter *evts* (make-mailbox "ui backend <- host"))
(defvar backend-thread nil)

(defun handle-cmd (cmd &optional args)
  (case cmd
    (:open
     (progn
       (stop-backend backend-thread)
       (sleep .1)
       (setf backend-thread (start-backend *evts*))
       (dispatch-cmd :init)))
    (:close
     (progn
       (stop-backend backend-thread t)
       (setf backend-thread nil)))

    (:start-scan
     (dispatch-cmd :start-scan))
    (:stop-scan
     (dispatch-cmd :stop-scan))

    (:connect
     (let ((address
             (make-address (gethash "address" (gethash "address" args))
                           (gethash "address_type" (gethash "address" args)))))
       (dispatch-cmd :connect address)))
    (:disconnect
     (dispatch-cmd :disconnect (gethash "conn" args)))

    (:att-read
     (dispatch-cmd :att-read
                   (gethash "conn" args)
                   (gethash "handle" args)))
    (:att-write
     (dispatch-cmd :att-write
                   (gethash "conn" args)
                   (gethash "handle" args)
                   (coerce (gethash "data" args) 'list)))
    (:att-notify
     (dispatch-cmd :att-notify
                   (gethash "conn" args)
                   (gethash "handle" args)
                   (coerce (gethash "data" args) 'list)))
  )
  (format nil "execute: ~A" cmd))

(jsonrpc:expose *server* "command"
                (lambda (args)
                  (format t "args: ~A~%" args)
                  (if (stringp args)
                      (cond
                        ((string= "open" args)
                         (handle-cmd :open))
                        ((string= "close" args)
                         (handle-cmd :close))

                        ((string= "start_scan" args)
                         (handle-cmd :start-scan))
                        ((string= "stop_scan" args)
                         (handle-cmd :stop-scan)))

                      (cond
                        ((gethash "connect" args)
                         (handle-cmd :connect (gethash "connect" args)))
                        ((gethash "disconnect" args)
                         (handle-cmd :disconnect (gethash "disconnect" args)))

                        ((gethash "att_read" args)
                         (handle-cmd :att-read (gethash "att_read" args)))
                        ((gethash "att_write" args)
                         (handle-cmd :att-write (gethash "att_write" args)))
                        ((gethash "att_notify" args)
                         (handle-cmd :att-notify (gethash "att_notify" args)))
                        ))))

;; TODO: make a "device" hashtable and clear it on start
(defparameter *scanned-devices* (make-hash-table))
(defparameter *last-scan-display* (get-internal-real-time))

(defun evt->json (evt)
  (log-dbg "UI-EVT: ~A" evt)
  (case (if (listp evt) (car evt) evt)
    (:display-scanned-devices
     (progn
       (make-scan-results *scanned-devices*)))

    (:gatt-server-table
     (make-server-discovered (nth 1 evt)))

    (:gatt-discovery-end
     (let* ((conn-handle (nth 1 evt))
            (table (nth 2 evt)))
       (make-client-discovered conn-handle table)))

    (:evt
     (case (car (cadr evt))
       (:le-scan-report
        (progn
          (accumulate-scan-reports *scanned-devices* (cadr evt))
          (when (> (- (get-internal-real-time) *last-scan-display*) 10000)
            (setf *last-scan-display* (get-internal-real-time))
            (queue *evts* :display-scanned-devices)
            nil)))

       (:le-enh-conn-complete
        (let* ((data (cadr (cadr evt)))
               (conn-handle (getf data :handle))
               (address-with-type (make-address
                                   (decode-c-int (getf data :peer-address) :u64)
                                   (getf data :peer-address-type))))

          ;; Kick off GATT discovery
          (dispatch-cmd :discover-gatt conn-handle)

          (make-conn-complete
           (list
            (getf address-with-type :type)
            (getf address-with-type :address))
           conn-handle)))

       (:disconnection-complete
        (destructuring-bind
            (&key handle &allow-other-keys)
            (cadr (cadr evt))
          (make-disconnected handle)))

       (:otherwise
        (progn
          (log-inf "EVENT: ~X" evt)
          "unknown-event"))
       ))

    (:acl
     (let ((att-packet (decode-att (cadr evt))))
       (when att-packet
         (log-inf "ATT RX: ~X" att-packet)
         (make-att-event (format nil "~X" att-packet))
         )))
     ))

(jsonrpc:expose *server* "get_event"
                ;; Only one concurrent client supported
                (lambda (args)
                  (declare (ignore args))
                  (log-inf ">>> Get event")
                  (let ((rsp))
                    (loop until rsp do
                      (setf rsp (evt->json (sb-concurrency:receive-message *evts*))))
                    (log-inf "<<< ~A" rsp)
                    rsp)))
