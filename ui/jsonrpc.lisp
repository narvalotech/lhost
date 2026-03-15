;; eval utils.lisp before
(ql:quickload '(:jsonrpc :usocket :yason))
(setf yason:*list-encoder* 'yason:encode-alist)
(setf yason:*parse-object-as* :alist)

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

(defun make-scan-result (address rssi name data &optional (decoded ""))
  (list (cons "scan_result"
              (list (cons "address"
                          (list (cons "address_type" (car address))
                                (cons "address" (cadr address))))
                    (cons "rssi" rssi)
                    (cons "name" name)
                    (cons "data" data)
                    (cons "decoded" (format nil "~X" decoded))))))

(defun make-conn-complete (address conn)
  (list (cons "conn_complete"
              (list (cons "conn_handle" conn)
                    (cons "address"
                          (list (cons "address_type" (car address))
                                (cons "address" (cadr address))))))))

(defun make-disconnected (handle)
  (list (cons "disconnected"
              (list (cons "conn_handle" handle)))))

(defparameter *cmds* (make-mailbox "ui backend -> host"))
(defparameter *evts* (make-mailbox "ui backend <- host"))
(defvar backend-thread nil)

(defun handle-cmd (cmd &optional args)
  (case cmd
    (:open
     (progn
       (setf backend-thread (start-backend *evts*))
       (dispatch-cmd :init)))
    (:close
     (progn
       (stop-backend backend-thread)
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
  )
  (format nil "execute: ~A" cmd))

(jsonrpc:expose *server* "command"
                (lambda (args)
                  (format t "args: ~A~%" args)
                  (cond
                    ((string= "open" args)
                     (handle-cmd :open))
                    ((string= "close" args)
                     (handle-cmd :close))

                    ((string= "start_scan" args)
                     (handle-cmd :start-scan))
                    ((string= "stop_scan" args)
                     (handle-cmd :stop-scan))

                    ;; ((gethash "connect" args)
                    ;;  (handle-cmd :connect (gethash "connect" args)))
                    ;; ((gethash "disconnect" args)
                    ;;  (handle-cmd :disconnect (gethash "disconnect" args)))

                    )))

(defun evt->json (evt)
  (log-dbg "UI-EVT: ~A" evt)
  (case (if (listp evt) (car evt) evt)
    (:evt
     (case (car (cadr evt))
       (:le-scan-report
        (destructuring-bind
            (&key address-type address timestamp rssi data)
            (decode-scan-report (cadr evt))
          (make-scan-result
           (list address-type address)
           rssi
           (extract-name (parse-ad data))
           (coerce data 'vector)
           (parse-ad data))))

       (:le-enh-conn-complete
        (let* ((data (cadr (cadr evt)))
               (conn-handle (getf data :handle))
               (address-with-type (make-address
                                   (decode-c-int (getf data :peer-address) :u64)
                                   (getf data :peer-address-type)))
               (address (getf address-with-type :address)))

          (make-conn-complete
           (list
            (getf address-with-type :type)
            (getf address-with-type :address))
           conn-handle)))

       (:disconnection-complete
        (destructuring-bind
            (&key status handle reason)
            (cadr evt)
          (make-disconnected
           handle)))

       (:otherwise
        (progn
          (log-inf "EVENT: ~X" evt)
          "unknown-event"))
       ))))

(jsonrpc:expose *server* "get_event"
                ;; Only one concurrent client supported
                (lambda (args)
                  (declare (ignore args))
                  (let ((evt (sb-concurrency:receive-message *evts*)))
                    (evt->json evt))))
