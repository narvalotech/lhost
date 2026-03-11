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

(defparameter *handle* #x0010)
(defun make-conn-complete (address)
  (list (cons "conn_complete"
              (list (cons "conn_handle" (incf *handle*))
                    (cons "address"
                          (list (cons "address_type" (car address))
                                (cons "address" (cadr address))))))))

(defun make-disconnected ()
  (let ((handle *handle*))
    (decf *handle*)
    (list (cons "disconnected"
                (list (cons "conn_handle" handle))))))

(defvar *send-connect* nil)
(defvar *send-disconnect* nil)
(defparameter *rssi* 0)
(defun make-rssi ()
  (- (random 100)))
(defparameter *n1*
  '("zealous"
    "big"
    "small"
    "fuzzy"))
(defparameter *n2*
  '("meerkat"
    "automobile"
    "thing"
    "ball"))
(defun make-name ()
  (format nil "~A-~A"
          (nth (random (length *n1*)) *n1*)
          (nth (random (length *n2*)) *n2*)))
(make-name)

;; Example: Returning a complex 'UserProfile' struct to Rust
(jsonrpc:expose *server* "get_event"
                (let ((addr #x00aA7DDA7114))
                  (lambda (args)
                    (declare (ignore args))
                    (sleep .1)
                    (cond
                      (*send-connect*
                       (let ((address *send-connect*))
                         (setf *send-connect* nil)
                         (make-conn-complete address)))

                      (*send-disconnect*
                       (progn
                         (setf *send-disconnect* nil)
                         (make-disconnected)))

                      (t
                       (make-scan-result
                        (list 1 (incf addr))
                        (make-rssi) (make-name)
                        #(1 2 3 4 5) "my-adv-data"))))))

(jsonrpc:expose *server* "connect"
                (lambda (args)
                  (setf *send-connect*
                        (list (gethash "address_type" (gethash "address" args))
                              (gethash "address" (gethash "address" args))))
                  (format nil "Connected to (~X) ~X"
                          (gethash "address_type" (gethash "address" args))
                          (gethash "address" (gethash "address" args)))))

(jsonrpc:expose *server* "disconnect"
                (lambda (args)
                  (setf *send-disconnect* (gethash "conn" args))
                  (format nil "Disconnected from ~X" *send-disconnect*)))

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
