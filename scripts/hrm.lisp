;; (require :host-utils)
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

(defun handle-ctrl-c (condition)
  (declare (ignore condition))
  (hci-log-write)
  (sb-ext:exit :code 130))

(defun process-ltk-request (hci evt)
  (declare (ignore hci evt))
  ;; uh-oh no keys
  nil)

(defun smp-process-pairing-req (conn data)
  ;; Send pairing failed all the time
  ;; do nothing
  nil)

;; (defun smp-process-pairing-req (conn data)
;;   ;; Send pairing failed all the time
;;   ;; do nothing
;;   ;; nil)
;;   (declare (ignore data))
;;   (log-inf "Telling central we can't pair right now")
;;   (setf (get-smp-context conn) (list :nopair t))
;;   (smp-make-packet :pairing-failed (list #x05)))

(defconstant +gatt-uuid-body-sensor-location+ #x2A38)

(defvar *heart-rate-bpm* 72)
(defvar *heart-rate-increasing* t)
(defparameter *heart-rate-subscribed* nil)
(defparameter *battery-subscribed* nil)

(defun make-heart-rate-measurement ()
  ;; uint8 heart rate with sensor contact supported and detected.
  (append
   (make-c-int :u8 #x06)
   (make-c-int :u8 *heart-rate-bpm*)))

(defun update-heart-rate ()
  (if *heart-rate-increasing*
      (incf *heart-rate-bpm*)
      (decf *heart-rate-bpm*))
  (when (>= *heart-rate-bpm* 90)
    (setf *heart-rate-increasing* nil))
  (when (<= *heart-rate-bpm* 72)
    (setf *heart-rate-increasing* t)))

(defun make-open-cccd-storage (subscription-callback)
  ;; CCCD that allows subscribe/unsubscribe without encryption.
  (let ((cccd-value '(0 0)))
    (list
     :read
     (lambda (conn handle)
       (declare (ignore conn handle)) cccd-value)
     :write
     (lambda (conn handle value)
       (declare (ignore conn handle))
       (log-inf "CCCD write: ~X" value)
       (setf cccd-value value)
       (funcall subscription-callback
                (and (consp value) (logbitp 0 (first value))))
       nil))))

(defun make-read-fn (value)
  (lambda (c h)
    (declare (ignore c))
    (log-inf "ATT VALUE READ: h ~X v ~X" h value)
    value))

(defparameter *battery-value* 95)

(defparameter *name* "TICKR X A23F")

(defparameter *gatts-table*
  (gatts-make-table
   (gatts-make-service +gatt-uuid-gap-service+)
   (gatts-make-char-decl +gatt-uuid-gap-device-name+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-gap-device-name+ (list :read (make-read-fn (to-c-string *name*))))
  (gatts-make-char-decl +gatt-uuid-gap-appearance+ (make-props '(:read)))
  ;; 0x0341 = Heart Rate Belt.
  (gatts-make-char-value +gatt-uuid-gap-appearance+ (list :read (make-read-fn (make-c-int :u16 #x0341))))

   (gatts-make-service +gatt-uuid-gatt-service+)
   (gatts-make-char-decl +gatt-uuid-gatt-service-changed+ (make-props '(:indicate)))
   (gatts-make-char-value +gatt-uuid-gatt-service-changed+ '())
   (gatts-make-cccd (make-cccd-storage))

   ;; Battery service
   (gatts-make-service #x180F)
   (gatts-make-char-decl #x2A19 (make-props '(:read :notify)))
   (gatts-make-char-value #x2A19 (list :read (lambda (c h)
                                                       (declare (ignore c h))
                                                       (make-c-int :u8 *battery-value*))))
   (gatts-make-cccd
    (make-open-cccd-storage
     (lambda (subscribed) (setf *battery-subscribed* subscribed))))

   ;; Heart Rate service
   (gatts-make-service +gatt-uuid-heart-rate-service+)
  (gatts-make-char-decl +gatt-uuid-heart-rate-measurement+ (make-props '(:read :notify)))
  (gatts-make-char-value +gatt-uuid-heart-rate-measurement+
                  (list :read (make-read-fn '(#x06 #x4B))))
   (gatts-make-cccd
    (make-open-cccd-storage
     (lambda (subscribed) (setf *heart-rate-subscribed* subscribed))))
   (gatts-make-char-decl +gatt-uuid-body-sensor-location+ (make-props '(:read)))
   ;; 0x01 = Chest.
   (gatts-make-char-value +gatt-uuid-body-sensor-location+
                          (list :read (make-read-fn (make-c-int :u8 #x01))))
   ))

(defparameter *stop-sensor* nil)

(defun inner-loop (hci conn-handle heart-rate-handle battery-handle)
  (update-heart-rate)
  (when *battery-subscribed*
    (notify hci conn-handle battery-handle (make-c-int :u8 *battery-value*)))
  (when *heart-rate-subscribed*
    (notify hci conn-handle heart-rate-handle (make-heart-rate-measurement)))
  (when (or *heart-rate-subscribed* *battery-subscribed*)
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
  ;; (hci-set-random-address hci #xE3A6671BFC5B)
  ;; (hci-set-random-address hci #x80bd636ac2b4)
  ;; (hci-set-random-address hci #xb4c26a63bd80) ; trolling motor
  ;; (hci-set-random-address hci #xD60BDA516DE1) ; hrm600
  )

(setf *bonds* (make-hash-table))

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

   (let ((heart-rate-handle
           (gatt-find-handle *gatts-table* +gatt-uuid-heart-rate-measurement+
                             :type :characteristic-value))
         (battery-handle
           (gatt-find-handle *gatts-table* #x2A19
                             :type :characteristic-value)))

     (loop do
       (progn

       (let ((conn-handle)
             ;; Shadow active-conns
             (*active-conns* '())
             (ads (list
                               (make-ad :flags '(#x06)) ; LE General discoverable, BR/EDR unsupported
                               (make-ad :class-uuid-16-complete
                                        (make-c-int :u16 +gatt-uuid-heart-rate-service+))
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

         ;; Send heart-rate notifications until the client disconnects
         (setf *heart-rate-bpm* 72
               *heart-rate-increasing* t
               *stop-sensor* nil
               *battery-value* 95
               *heart-rate-subscribed* nil
               *battery-subscribed* nil)
         (let ((disconnected nil))
           (loop until (or disconnected *stop-sensor*) do
             (drain-rxq hci)
             ;; Check for remote disconnect BEFORE do-idle-work consumes the event
             (if (receive-rxq hci (evt? :disconnection-complete))
                 (progn
                   (log-inf (format nil "Client disconnected from ~A" conn-handle))
                   (setf disconnected t))
                 (inner-loop hci conn-handle heart-rate-handle battery-handle)))
           (unless disconnected
             (log-inf (format nil "Disconnecting from ~A" conn-handle))
             (hci-disconnect hci conn-handle)
             (wait-for-disconn hci))
            )))))

     (log-dbg (format nil "HCI: ~X" hci))
     (log-inf "================ exit ===============")
     )))

(setf *stop-sensor* t)
(stop-hci (getf *controller* :stop-signal))
(hci-log-write)
