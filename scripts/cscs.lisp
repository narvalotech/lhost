(require 'asdf)
(unless (find-package :quicklisp)
  (let ((quicklisp-init
          (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp setup not found at ~A" quicklisp-init))
    (load quicklisp-init)))
(asdf:load-asd (merge-pathnames "../systems/host.asd" *load-truename*))
(asdf:load-system :host)
(in-package :host)

(defparameter *controller* (make-controller))
(defvar *packetizer-path* "COM6")

(defun process-ltk-request (hci evt)
  (declare (ignore hci evt))
  ;; uh-oh no keys
  nil)

(defun smp-process-pairing-req (conn data)
  ;; Send pairing failed all the time
  ;; do nothing
  ;; nil)
  (declare (ignore data))
  (log-inf "Telling central we can't pair right now")
  (setf (get-smp-context conn) (list :nopair t))
  (smp-make-packet :pairing-failed (list #x05)))

;; Cycling Speed and Cadence Service UUIDs (Bluetooth Assigned Numbers)
(defconstant +gatt-uuid-csc-service+     #x1816)
(defconstant +gatt-uuid-csc-measurement+ #x2A5B)
(defconstant +gatt-uuid-csc-feature+     #x2A5C)
(defconstant +gatt-uuid-sensor-location+ #x2A5D)

;; CSC state: cumulative crank revolutions and last crank event time (1/1024 s)
(defvar *csc-crank-revs* 0)
(defvar *csc-wheel-revs* 0)
(defvar *csc-crank-event-time* 0)

(defun make-csc-measurement ()
  ;; CSC Measurement (0x2A5B) encoding:
  ;; Flags  (u8):  bit 0 = wheel data present, bit 1 = crank data present
  ;; Cumulative Crank Revolutions (u16)
  ;; Last Crank Event Time        (u16, 1/1024 s resolution)
  (append
   (make-c-int :u8  #x02)                   ; flags: crank + wheel revolution data present
   (make-c-int :u16 *csc-crank-revs*)
   (make-c-int :u16 *csc-crank-event-time*)))

(defun update-csc-state ()
  (incf *csc-crank-event-time* 1024)
  (incf *csc-crank-revs*))

(defparameter *subscribed* nil)

(defun make-open-cccd-storage ()
  ;; CCCD that allows subscribe/unsubscribe without encryption.
  ;; Needed here because pairing is rejected, so the link is never encrypted.
  (list
   :read
   (lambda (conn handle)
     (declare (ignore conn handle)) '(0 0))
   :write
   (lambda (conn handle value)
     (declare (ignore conn handle))
     (log-inf "CCCD write: ~X" value)
     (setf *subscribed* t)
     nil)))

(defun make-read-fn (value)
  (lambda (c h)
    (declare (ignore c))
    (log-inf "ATT VALUE READ: h ~X v ~X" h value)
    value))

(defparameter *battery-value* 80)

;; (defparameter *name* "myCadence")
(defparameter *name* "justdont")

(defparameter *gatts-table*
  (gatts-make-table
   (gatts-make-service +gatt-uuid-gap-service+)
   (gatts-make-char-decl +gatt-uuid-gap-device-name+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-gap-device-name+ (list :read (make-read-fn (to-c-string *name*))))

   (gatts-make-char-decl +gatt-uuid-gap-appearance+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-gap-appearance+ (list :read (make-read-fn (make-c-int :u16 #x0485))))

   (gatts-make-char-decl +gatt-uuid-gap-ppcp+ (make-props '(:read)))
   ;; 0x0485 = Cycling Speed and Cadence Sensor (Bluetooth Assigned Numbers)
   (gatts-make-char-value +gatt-uuid-gap-ppcp+ (list :read (make-read-fn '(06 00 06 00 00 00 58 02))))

   (gatts-make-service +gatt-uuid-gatt-service+)
   (gatts-make-char-decl +gatt-uuid-gatt-service-changed+ (make-props '(:indicate)))
   (gatts-make-char-value +gatt-uuid-gatt-service-changed+ '())
   (gatts-make-cccd (make-cccd-storage))

   ;; Battery service
   (gatts-make-service #x180F)
   (gatts-make-char-decl #x2A19 (make-props '(:notify)))
   (gatts-make-char-value #x2A19 (list :read (make-read-fn (make-c-int :u8 80))))
   (gatts-make-cccd (make-open-cccd-storage))

   (gatts-make-service +gatt-uuid-csc-service+)
   ;; CSC Measurement (mandatory): Notify only per CSCS spec
   (gatts-make-char-decl +gatt-uuid-csc-measurement+ (make-props '(:notify)))
   (gatts-make-char-value +gatt-uuid-csc-measurement+ '())
   (gatts-make-cccd (make-open-cccd-storage))
   ;; CSC Feature (mandatory): bit 1 = Crank Revolution Data Supported
   (gatts-make-char-decl +gatt-uuid-csc-feature+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-csc-feature+ (list :read (make-read-fn (make-c-int :u16 #x0002))))
   ;; Sensor Location: 0x06 = Right Crank. 00 -> other
   (gatts-make-char-decl +gatt-uuid-sensor-location+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-sensor-location+ (list :read (make-read-fn (make-c-int :u8 #x00))))
   ))

(defparameter *stop-sensor* nil)

(defun inner-loop (hci conn-handle csc-meas-handle)
  (update-csc-state)
  (when *subscribed*
    (notify hci conn-handle csc-meas-handle (make-csc-measurement))
    (sleep 0.75))
  (do-idle-work hci)
  (sleep .001))

(format t "~A" (gattc-print *gatts-table*))

(defun init-controller (hci)
  (hci-reset hci)
  (hci-read-buffer-size hci)
  (hci-allow-all-the-events hci)
  ;; (hci-set-random-address hci #xF131724921B9)
  (hci-set-random-address hci #xC2222267890A)
  )

(setf *bonds* (make-hash-table))

(defun handle-ctrl-c (condition)
  (declare (ignore condition))
  (hci-log-write)
  (sb-ext:exit :code 130))

(handler-bind ((sb-sys:interactive-interrupt #'handle-ctrl-c))
  (start-hci
   *packetizer-path*
   (getf *controller* :tx-mailbox)
   (getf *controller* :rx-mailbox)
   (getf *controller* :stop-signal))
  (time
   (let ((hci *controller*))
     (hci-log-reset)
     (log-inf "================ enter ===============")
     (log-inf (format nil "Our table: ~A~%" (gattc-print *gatts-table*)))

     ;; (setf *bonds* (make-hash-table))     ; comment to persist the bonds
     (disable-mitm)
     (init-controller hci)

     (let ((csc-meas-handle
             (gatt-find-handle *gatts-table* +gatt-uuid-csc-measurement+
                               :type :characteristic-value)))

       (loop do
         (let ((conn-handle)
               ;; Shadow active-conns
               (*active-conns* '())
               (ads (list
                     (make-ad :flags '(#x06)) ; LE General discoverable, BR/EDR unsupported
                     (make-ad :class-uuid-16-complete
                              (make-c-int :u16 +gatt-uuid-csc-service+))
                     (make-ad-name *name*)))
               )
           (start-advertising hci (copy-tree ads))

           (log-inf (format nil "Wait for connection"))
           (let ((conn-evt (wait-for-conn hci)))
             (setf conn-handle (getf conn-evt :handle))
             (setf (getf (getf *active-conns* conn-handle) :our-address)
                   (make-address (getf hci :random-address) #x01))
             (setf (getf (getf *active-conns* conn-handle) :address)
                   (getf conn-evt :address)))

           (setf (get-smp-context conn-handle) '())

           ;; Handle pairing attempt (we reject it, but the loop must drain events)
           ;; (loop until
           ;;       (get-smp-context conn-handle)
           ;;       do
           ;;       (progn
           ;;         (drain-rxq hci)
           ;;         (do-idle-work hci)
           ;;         (sleep .1)))

           ;; Send CSC notifications until the client disconnects
           (setf *csc-crank-revs* 0
                 *csc-crank-event-time* 0
                 *stop-sensor* nil
                 *battery-value* 100
                 *subscribed* nil)
           (let ((disconnected nil))
             (loop until (or disconnected *stop-sensor*) do
               (drain-rxq hci)
               ;; Check for remote disconnect BEFORE do-idle-work consumes the event
               (if (receive-rxq hci (evt? :disconnection-complete))
                   (progn
                     (log-inf (format nil "Client disconnected from ~A" conn-handle))
                     (setf disconnected t))
                   (inner-loop hci conn-handle csc-meas-handle)))
             (unless disconnected
               (log-inf (format nil "Disconnecting from ~A" conn-handle))
               (hci-disconnect hci conn-handle)
               (wait-for-disconn hci))))))

     (log-dbg (format nil "HCI: ~X" hci))
     (log-inf "================ exit ===============")
     )))

(setf *stop-sensor* t)
(stop-hci (getf *controller* :stop-signal))
(hci-log-write)
