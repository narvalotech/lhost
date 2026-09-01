(ql:quickload :cl-protobufs)
(ql:quickload :prototest)
(ql:quickload :host)

(defpackage #:ptest
  (:use #:common-lisp)
  (:use #:host)
  (:local-nicknames (#:p #:cl-protobufs.prototest)
                    (#:clp #:cl-protobufs)))
(in-package :ptest)

(defun pyhex (bytes)
  "Output in python-compatible hex format"
  (with-output-to-string (s)
    (loop for byte across bytes
          for first = t then nil
          do (unless first (write-char #\Space s))
             (format s "~(~2,'0x~)" byte))))

(defparameter *tlist*
  (list
   (p:make-tee :range 100 :dist-x -10 :dist-y -400)
   (p:make-tee :range 10 :dist-x -10)
   (p:make-tee :range 100 :dist-y -120)))

(defparameter *o1*
  (p:make-omnibus
   :state :state-running
   :flags #x01
   :objects *tlist*))

(pyhex (clp:serialize-to-bytes *o1*))

(defun make-value (elapsed-time)
  (declare (ignore elapsed-time))
  (coerce
   (clp:serialize-to-bytes *o1*)
   'list))
(export 'make-value)

;; replay!
(defparameter *fake-values* '((0 1 2 3 4)
                              (1 2 3 4 5)
                              (6 4 3 2 1)))

(defun ptest:make-value (elapsed-time)
  (declare (ignore elapsed-time))
  (when *fake-values*
    (pop *fake-values*)))


;;;;;;;;;;; Bluetooth device start ;;;;;;;;;;;;;
(in-package :host)

(defparameter *name* "Omnibus")

(defparameter +gatt-nus-svc+ #x6E400001B5A3F393E0A9E50E24DCCA9E)
(defparameter +gatt-nus-tx+ #x6E400003B5A3F393E0A9E50E24DCCA9E)
(defparameter +gatt-nus-rx+ #x6E400002B5A3F393E0A9E50E24DCCA9E)

(defun make-read-fn (value)
  (lambda (c h)
    (declare (ignore c))
    (log-inf "ATT VALUE READ: h ~X v ~X" h value)
    value))

(defun make-cccd-storage-cb (subscription-callback)
  (let ((cccd-db))
    (list
     :read
     (lambda (conn handle)
       (declare (ignore handle))
       (getf cccd-db (get-address conn)))

     :write
     (lambda (conn handle value)
       (declare (ignore handle))
       (progn
         (setf (getf cccd-db (get-address conn)) value)
         (funcall subscription-callback
                  (and (consp value) (logbitp 0 (first value))))
         nil))
     )))

(defparameter *value-subscribed* nil)
(defparameter *gatts-table*
  (gatts-make-table
   ;; Mandatory GAP service
   (gatts-make-service +gatt-uuid-gap-service+)
   (gatts-make-char-decl +gatt-uuid-gap-device-name+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-gap-device-name+ (list :read (make-read-fn (to-c-string *name*))))

   (gatts-make-char-decl +gatt-uuid-gap-appearance+ (make-props '(:read)))
   ;; FIXME
   (gatts-make-char-value +gatt-uuid-gap-appearance+ (list :read (make-read-fn (make-c-int :u16 #x0485))))

   (gatts-make-char-decl +gatt-uuid-gap-ppcp+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-gap-ppcp+ (list :read (make-read-fn '(06 00 06 00 00 00 58 02))))

   ;; Mandatory GATT service
   (gatts-make-service +gatt-uuid-gatt-service+)
   (gatts-make-char-decl +gatt-uuid-gatt-service-changed+ (make-props '(:indicate)))
   (gatts-make-char-value +gatt-uuid-gatt-service-changed+ '())

   ;; User services

   ;; FIXME: replace with real char
   (gatts-make-service +gatt-nus-svc+)
   (gatts-make-char-decl +gatt-nus-tx+ (make-props '(:read :notify)))
   (gatts-make-char-value +gatt-nus-tx+ (list :read #'read-spy))
   (gatts-make-cccd (make-cccd-storage-cb (lambda (subscribed) (setf *value-subscribed* subscribed))))
   (gatts-make-char-decl +gatt-nus-rx+ (make-props '(:write :write-no-rsp)))
   (gatts-make-char-value +gatt-nus-rx+ (list :write #'write-spy))
   ))

(defun init-controller (hci)
  ;; Redefine to set the address
  (hci-reset hci)
  (hci-read-buffer-size hci)
  (hci-allow-all-the-events hci)
  (hci-set-random-address hci #xC2222267890A))

(setf *bonds* (make-hash-table))

(defparameter *controller* (make-controller))
(defvar *packetizer-path* (get-hci-path))

(defun handle-ctrl-c (condition)
  (declare (ignore condition))
  (hci-log-write)
  (sb-ext:exit :code 130))

(defun inner-loop (hci conn-handle value-handle elapsed-time)
  (when *value-subscribed*
    (notify hci conn-handle value-handle (ptest:make-value elapsed-time))
    (sleep .5))
  (do-idle-work hci)
  (sleep .001))

(defparameter *stop-sensor* nil)

(handler-bind ((sb-sys:interactive-interrupt #'handle-ctrl-c))
  (setf *current-log-level* :dbg)

  (start-hci
   *packetizer-path*
   (getf *controller* :tx-mailbox)
   (getf *controller* :rx-mailbox)
   (getf *controller* :stop-signal))

  (let ((hci *controller*))
    (hci-log-reset)
    (log-inf "================ enter ===============")
    (log-inf (format nil "Our table: ~A~%" (gattc-print *gatts-table*)))

    ;; (setf *bonds* (make-hash-table))     ; comment to persist the bonds
    (disable-mitm)
    (init-controller hci)

    (let ((value-handle
            (gatt-find-handle *gatts-table* +gatt-nus-tx+
                              :type :characteristic-value)))
      (loop do
        (let ((conn-handle)
              ;; Shadow active-conns
              (*active-conns* '())
              (ads (list
                    (make-ad :flags '(#x06)) ; LE General discoverable, BR/EDR unsupported
                    (make-ad :class-uuid-16-complete
                             (make-c-int :u16 +gatt-uuid-heart-rate-service+))
                    (make-ad-name *name*))))
          (start-advertising hci (copy-tree ads))

          (log-inf (format nil "Wait for connection"))
          (let ((conn-evt (wait-for-conn hci)))
            (setf conn-handle (getf conn-evt :handle))
            (setf (getf (getf *active-conns* conn-handle) :our-address)
                  (make-address (getf hci :random-address) #x01))
            (setf (getf (getf *active-conns* conn-handle) :address)
                  (getf conn-evt :address)))

          (log-inf (format nil "Connected"))

          (setf (get-smp-context conn-handle) '()) ; Reset pairing state machine
          (att-set-mtu hci conn-handle 150)

          (let ((disconnected nil))
            (setf *value-subscribed* nil)
            (loop until (or disconnected *stop-sensor*) do
              (drain-rxq hci)
              ;; Check for remote disconnect BEFORE do-idle-work consumes the event
              (if (receive-rxq hci (evt? :disconnection-complete))
                  (progn
                    (log-inf (format nil "Client disconnected from ~A" conn-handle))
                    (setf disconnected t))
                  ;; TODO: send elapsed time since connection
                  (inner-loop hci conn-handle value-handle 0)))
            (unless disconnected
              (log-inf (format nil "Disconnecting from ~A" conn-handle))
              (hci-disconnect hci conn-handle)
              (wait-for-disconn hci))
            ))))

    (log-dbg (format nil "HCI: ~X" hci))
    (log-inf "================ exit ===============")
    ))

(stop-hci (getf *controller* :stop-signal))
(hci-log-write)
