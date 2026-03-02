(ql:quickload '(:jsonrpc :usocket :yason))
(setf yason:*list-encoder* 'yason:encode-alist)

(ql:quickload :bordeaux-threads)
(defparameter *server* (jsonrpc:make-server))
;; TODO: move to raw sockets
(bt:make-thread
 (lambda ()
   (jsonrpc:server-listen *server* :port 55000 :mode :tcp))
 :name "JRPC server")

;; Define a simple method
(jsonrpc:expose *server* "echo"
                (lambda (args)
                  (format t "Lisp received: ~A~%" (gethash "message" args))
                  (format nil "Lisp received: ~A~%" (gethash "message" args))
                  ))

;; Example: Returning a complex 'UserProfile' struct to Rust
(jsonrpc:expose *server* "get_event"
                (lambda (args)
                  (declare (ignore args))
                  (sleep 2)
                  (format nil "hola")))

(jsonrpc:expose *server* "connect"
                (lambda (args)
                  (format nil "Connected to (~X) ~X"
                          (gethash "address_type" (gethash "address" args))
                          (gethash "address" (gethash "address" args)))))

(defparameter *events* nil)
(defun get-events ()
  (when *events*
    (pop *events*)))

(jsonrpc:expose *server* "get_events"
                (lambda (args)
                  (declare (ignore args))
                  (format nil "~A" (get-events))))

;; Slint MVP:
;; connect/disconnect to a device
;;
;; TODO:
;; - cmd:
;;   - scan/connect/disconnect
;; - evt:
;;   - scan results
;;   - conn complete
;; - state:
;;   - list of scanned devices
;;   - list of active connections
