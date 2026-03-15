(ql:quickload '(:jsonrpc :usocket :yason))
(setf yason:*list-encoder* 'yason:encode-alist)

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
(defun make-conn-complete (address conn)
  (list (cons "conn_complete"
              (list (cons "conn_handle" conn)
                    (cons "address"
                          (list (cons "address_type" (car address))
                                (cons "address" (cadr address))))))))

(defun make-disconnected ()
  (let ((handle *handle*))
    (decf *handle*)
    (list (cons "disconnected"
                (list (cons "conn_handle" handle))))))

(defun fake-type (i)
  (let ((types '("service" "characteristic_declaration" "characteristic_value" "characteristic_descriptor")))
    (nth (mod i (length types)) types)))

(defun make-fake-gatt ()
  (list
   (cons
    "attributes"
    (coerce
     (loop for i from 0 to 10 collect
                              (list
                               (cons "handle" i)
                               (cons "att_type" (fake-type i))
                               (cons "uuid16" (+ i #x2a28))
                               (cons "uuid128" 0)))
     'vector))))

(yap (make-fake-gatt))

(defun make-server-discovered ()
  (list (cons "server_discovered"
              (list
               (cons "address"
                     (list (cons "address_type" 0)
                           (cons "address" 0)))
               (cons "conn_handle" #xFFFF)
               (cons "gatt" (make-fake-gatt))))))

(defun make-peer-device (address conn)
  (list (cons "discovered"
              (list
               (cons "address"
                     (list (cons "address_type" (car address))
                           (cons "address" (cadr address))))
               (cons "conn_handle" conn)
               (cons "gatt" (make-fake-gatt))))))

(defvar *send-connect* nil)
(defvar *send-gatt* nil)
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

(defparameter *send-own-gatt* nil)

;; Example: Returning a complex 'UserProfile' struct to Rust
(jsonrpc:expose *server* "get_event"
                (let ((addr #x00aA7DDA7114))
                  (lambda (args)
                    (declare (ignore args))
                    (sleep 1)
                    (cond
                      (*send-connect*
                       (let ((address *send-connect*))
                         (setf *send-connect* nil)
                         (incf *handle*)
                         (setf *send-gatt* (make-peer-device address *handle*))
                         (make-conn-complete address *handle*)))

                      (*send-gatt*
                       (let ((gatt *send-gatt*))
                         (setf *send-own-gatt* t)
                         (setf *send-gatt* nil)
                         gatt))

                      (*send-own-gatt*
                       (progn
                         (setf *send-own-gatt* nil)
                         (make-server-discovered)))

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

(defun handle-cmd-connect (args)
  (setf *send-connect*
        (list (gethash "address_type" (gethash "address" args))
              (gethash "address" (gethash "address" args))))
  (format nil "Connected to (~X) ~X"
          (gethash "address_type" (gethash "address" args))
          (gethash "address" (gethash "address" args))))

(defun handle-cmd-disconnect (args)
  (setf *send-disconnect* (gethash "conn" args))
  (format nil "Disconnected from ~X" *send-disconnect*)))

(jsonrpc:expose *server* "command"
                (lambda (args)
                  (format t "args: ~A" args)
                  (cond
                    ((gethash "connect" args)
                     (handle-cmd-connect (gethash "connect" args)))
                    ((gethash "disconnect" args)
                     (handle-cmd-disconnect (gethash "disconnect" args))))))

(defparameter *events* nil)
(defun get-events ()
  (when *events*
    (pop *events*)))

(jsonrpc:expose *server* "get_events"
                (lambda (args)
                  (declare (ignore args))
                  (format nil "~A" (get-events))))
