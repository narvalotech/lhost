;; Load host.lisp first

(defparameter *controller* (make-controller))
(start-hci
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

   (let ((conn-peripheral)
         (conn-central)
         ;; Shadow active-conns
         (*active-conns* '())
         )

     (log-inf "#######################################")
     (log-inf (format nil "Wait for connection to peripheral"))

     (hci-set-scan-enable hci t)
     (let ((address (wait-for-scan-report hci (lambda (x) (name? "bob" x)))))
       (hci-set-scan-enable hci nil)
       (hci-create-connection hci (copy-tree address)))
     (let ((conn-evt (wait-for-conn hci)))
       (setf conn-peripheral (getf conn-evt :handle))
       (setf (getf (getf *active-conns* conn-peripheral) :our-address)
             (make-address (getf hci :random-address) #x01))
       (setf (getf (getf *active-conns* conn-peripheral) :address)
             (getf conn-evt :address)))

     (setf (get-smp-context conn-peripheral) '())
     ;; Pair peripheral
     (log-inf "#######################################")
     (log-inf "Pairing peripheral")
     (start-security hci conn-peripheral)
     ;; (smp-send-pairing-req hci conn-peripheral)

     (wait-for-encryption hci conn-peripheral)

     (log-inf "#######################################")
     (log-inf (format nil "Wait for connection to central"))

     (start-advertising hci (list
                             (make-ad :flags '(#x01)) ; LE General discoverable
                             (make-ad :class-uuid-16-incomplete
                                      (make-c-int :u16 +gatt-uuid-heart-rate-service+))
                             (make-ad-name "alice")))

     (let ((conn-evt (wait-for-conn hci)))
       (setf conn-central (getf conn-evt :handle))
       (setf (getf (getf *active-conns* conn-central) :our-address)
             (make-address (getf hci :random-address) #x01))
       (setf (getf (getf *active-conns* conn-central) :address)
             (getf conn-evt :address)))

     (setf (get-smp-context conn-central) '())

     ;; Pair central
     (let ((*bonds* (make-hash-table)))
       (log-inf "#######################################")
       (log-inf "Pairing central")
       (smp-send-security-req hci conn-central)
       (wait-for-encryption hci conn-central))

     ;; Process backed-up ATT packets
     (log-inf "#######################################")
     (log-inf "Drain queue")
     (start-mitm conn-central conn-peripheral)
     (drain-mitm-att-queue hci)
     (drain-mitm-signalling-queue hci)

     ;; At this point, ATT packets are transmitted to the other
     ;; connection.

     (log-inf (format nil "Active conns: ~X" *active-conns*))

     (log-inf "#######################################")

     ;; Sleep for a while, but still process packets
     (loop for i from 0 to 100 do
       (progn
         (drain-rxq hci)
         (do-idle-work hci)
         (sleep .1)))

     (log-inf (format nil "Disconnecting from conn-peripheral ~A" conn-peripheral))
     (hci-disconnect hci conn-peripheral)
     (wait-for-disconn hci)
     )

   (log-dbg (format nil "HCI: ~X" hci))
   (log-inf "================ exit ===============")
   ))

(stop-hci (getf *controller* :stop-signal))
(hci-log-write)
