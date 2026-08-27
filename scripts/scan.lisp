(require 'asdf)
(require :host)
(in-package :host)

(defvar *packetizer-path* "/dev/ttyACM0")
(defparameter +ftms-uuid+ #x1826)

(defparameter *controller* (make-controller))
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

   (setf *bonds* (make-hash-table))     ; comment to persist the bonds
   (disable-mitm)
   (arm-mitm)

   (init-controller hci)
   (hci-set-scan-param hci)

   (log-inf "#######################################")
   (log-inf (format nil "Wait for connection to peripheral"))

   (hci-set-scan-enable hci t)
   (loop
     (let ((address (wait-for-scan-report hci (lambda (x) (uuid? +ftms-uuid+ x)))))
       (log-inf "Found device: ~X ~X" address (decode-c-int (getf address :address))))
     )))

(stop-hci (getf *controller* :stop-signal))
(hci-log-write)
