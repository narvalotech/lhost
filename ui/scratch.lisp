(ql:quickload '(:jsonrpc :usocket :yason))
(setf yason:*list-encoder* 'yason:encode-alist)

(defvar *server* (jsonrpc:make-server))

;; Define a simple method
(jsonrpc:expose *server* "echo"
                (lambda (args)
                  (format t "Lisp received: ~A~%" (gethash "message" args))
                  (format nil "Lisp received: ~A~%" (gethash "message" args))
                  ))

;; Example: Returning a complex 'UserProfile' struct to Rust
(jsonrpc:expose *server* "get_user"
                (lambda (args)
                  (list (cons "id" (gethash "id" args))
                        (cons "name" "Jon")
                        (cons "roles" #("admin" "developer")))))

;; TODO: move to raw sockets
(jsonrpc:server-listen *server* :port 55000 :mode :tcp)
