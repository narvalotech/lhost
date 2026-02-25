(ql:quickload '(:jsonrpc :usocket))

(defvar *server* (jsonrpc:make-server))

;; Define a simple method
(jsonrpc:expose *server* "echo"
                (lambda (args)
                  (format t "Lisp received: ~A~%" (gethash "message" args))))
;; (jsonrpc:expose *server* "sum" (lambda (args) (reduce #'+ args)))

;; Start the server on a specific port
(jsonrpc:server-listen *server* :port 55000 :mode :tcp)

;; ;; client
;; (defvar *client* (jsonrpc:make-client))
;; (jsonrpc:client-connect *client* :url "http://127.0.0.1:55000" :mode :tcp)
;; (jsonrpc:call *client* "echo" "o hai")

;; (jsonrpc:call *client* "sum" '(10 20) :timeout 1.0)
