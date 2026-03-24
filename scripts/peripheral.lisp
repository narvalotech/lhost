(defparameter *controller* (make-controller))
(defvar *packetizer-path* "/dev/ttyACM0")

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
   (init-controller hci)

   (hci-set-scan-param hci)
   (hci-set-scan-enable hci t)

   (let ((conn-handle)
         ;; Shadow active-conns
         (*active-conns* '())
         (gatts-hr-handle
           (gatt-find-handle *gatts-table* +gatt-uuid-heart-rate-measurement+))
         (address (wait-for-scan-report hci (lambda (x) (name? "bob" x)))))

     (hci-set-scan-enable hci nil)
     (hci-create-connection hci (copy-tree address))

     (format t "CCCD before: ~X~%"
             (read-cccd conn-handle *gatts-table* gatts-hr-handle))

     (log-inf (format nil "Wait for connection"))
     (let ((conn-evt (wait-for-conn hci)))
       (setf conn-handle (getf conn-evt :handle))
       (setf (getf (getf *active-conns* conn-handle) :our-address)
             (make-address (getf hci :random-address) #x01))
       (setf (getf (getf *active-conns* conn-handle) :address)
             (getf conn-evt :address)))

     (setf (get-smp-context conn-handle) '())

     ;; Start pairing
     (smp-send-pairing-req hci conn-handle)
     (wait-for-encryption hci conn-handle)

     (log-inf "Discovering peer table")
     (let ((gattc-table (gattc-discover hci conn-handle)))
       (log-inf (format nil "Discovered: ~A~%" (gattc-print gattc-table)))
       (setf *test* gattc-table)
       (log-inf (format nil "Read GAP Device Name: ~A"
                        (from-c-string
                         (read-gap-name hci conn-handle gattc-table))))

       (log-inf "Subscribing")
       (gattc-subscribe hci conn-handle gattc-table
                        +gatt-uuid-heart-rate-measurement+))

     (log-inf (format nil "Active conns: ~X" *active-conns*))

     ;; Sleep for a while, but still process packets
     (loop for i from 0 to 10 do
       (progn
         (drain-rxq hci)
         (do-idle-work hci)
         (sleep .1)))

     (log-inf (format nil "Disconnecting from conn-handle ~A" conn-handle))
     (hci-disconnect hci conn-handle)
     (wait-for-disconn hci)
     )

   (log-dbg (format nil "HCI: ~X" hci))
   (log-inf "================ exit ===============")
   ))

(stop-hci (getf *controller* :stop-signal))
(hci-log-write)
