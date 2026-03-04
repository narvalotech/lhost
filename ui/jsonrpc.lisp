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

(defun make-scan-result (address rssi name data &optional (decoded ""))
  (list (cons "scan_result"
              (list (cons "address"
                          (list (cons "address_type" (car address))
                                (cons "address" (cadr address))))
                    (cons "rssi" rssi)
                    (cons "name" name)
                    (cons "data" data)
                    (cons "decoded" decoded)))))

;; Example: Returning a complex 'UserProfile' struct to Rust
(jsonrpc:expose *server* "get_event"
                (lambda (args)
                  (declare (ignore args))
                  (sleep .5)
                  (make-scan-result
                   '(1 #x00aA7DDA7114)
                   -90 "hello from lisp"
                   #(1 2 3 4 5) "my-adv-data")))

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
