;; eval utils.lisp before
(ql:quickload "nodgui")
;; (nodgui.demo:demo)
(setf nodgui:*default-theme* "default")
(sb-ext:add-package-local-nickname :ng :nodgui)

(defun make-a-sign (byte)
  (if (logbitp 7 byte)
      (- byte (ash 1 8))
      byte))

(defun decode-scan-report (evt)
  ;; TODO: handle multiple reports
  (let ((report (nth 0 (getf (nth 1 evt) :reports))))
    (list :address-type (getf report :address-type)
          :address (decode-c-int (getf report :address))
          :timestamp (get-internal-real-time)
          :rssi (make-a-sign (getf report :rssi))
          :data (getf report :data))))

(defun merge-plist (a b)
  (loop for (key val) on b by #'cddr
        do (setf (getf a key) val))
  a)

(defun merge-reports (dest decoded)
  (let ((parsed (parse-ad (getf decoded :data)))
        (existing (getf (gethash (getf decoded :address) dest) :parsed)))
    (setf (getf (gethash (getf decoded :address) dest) :parsed)
          (if existing
              (merge-plist existing parsed)
              parsed))))

(defun accumulate-scan-reports (dest report)
  ;; Track the last 20 reports for each address
  (let ((decoded (decode-scan-report report)))
    (push decoded (getf (gethash (getf decoded :address) dest) :reports))
    (nbutlast (getf (gethash (getf decoded :address) dest) :reports) 20)

    ;; Parse and merge reports
    (merge-reports dest decoded)
    nil))

(defun extract-name (parsed)
  (let* ((encoded-name (or (getf parsed :name-complete)
                           (getf parsed :name-short))))
    (ignore-errors
     (if encoded-name
         (from-c-string encoded-name)
         ""))))

(defun make-device-list (devices-dict)
  (let ((devices))
    (maphash (lambda (address data)
               (declare (ignore address))
               (let* ((report (car (getf data :reports)))
                      (name (extract-name (getf data :parsed))))
                 (push (append report (list :name name)) devices)))
             devices-dict)
    devices))

(defun print-addr (address)
  (format nil "~{~2,'0X~^:~}"
          (subseq (make-c-int :u64 address t) 2)))

(print-addr #xF89DC52A5C01)
 ; => "F8:9D:C5:2A:5C:01"

(defun decode-mac (address-string)
  (ignore-errors
   (parse-integer (remove #\: address-string) :radix 16)))

;; look ma! end-to-end testing!
(print-addr (decode-mac (print-addr #xF89DC52A5C01)))
 ; => "F8:9D:C5:2A:5C:01"

(defun make-button (frame text command)
  (make-instance 'ng:button
                 :master frame
                 :text text
                 :command command))

(defun get-selected-device (treeview)
  (let ((sel (ng:treeview-get-selection treeview)))
    (when sel
      (list
       :address
       (read-from-string
        (slot-value (car sel) 'ng:id))
       :values
       (slot-value (car sel) 'ng:column-values)))))

(defun queue (mailbox event)
  (when event
    (sb-concurrency:send-message mailbox event)))

(defun make-menuitem (master text q command-id &optional accelerator state)
  (apply #'make-instance
         (append
          (list 'ng:menubutton :master master :text text
                               :command (lambda () (queue q command-id)))
          (when state
            (list :state state))
          (when accelerator
            ;; Bind the keyboard event
            (ng:bind (ng:root-toplevel) accelerator (lambda (e)
                                                      (declare (ignore e))
                                                      (queue q command-id)))
            ;; Display the keyboard shortcut in the menu
            (list
             :accelerator accelerator)))))

(defun sort-scan (by devices)
  (sort devices
        (case by
          (:recent (lambda (a b) (< (getf a :timestamp) (getf b :timestamp))))
          (:rssi (lambda (a b) (> (getf a :rssi) (getf b :rssi))))
          (:name (lambda (a b) (string-lessp (getf a :name) (getf b :name))))
          (otherwise (lambda (a b) (string-lessp (getf a :name) (getf b :name)))))))

(defun filter-scan (name-filter devices)
  (if (zerop (length name-filter))
      devices
      (remove-if-not (lambda (device)
                       ;; Case-insensitive search
                       (search name-filter (getf device :name) :test #'equalp))
                     devices)))

(defun log-to-widget (textview line)
  (ng:configure textview :state :normal)
  (ng:append-text textview line)
  (ng:see textview "end")
  (ng:configure textview :state :disabled)
  ;; also log to stderr in case UI crashes
  (write-string line *error-output*))

(defun make-connection-tab (activity-frame address)
  (let ((treeview
          (make-instance 'ng:scrolled-treeview
                         :columns (list "type" "uuid" "value")
                         :master activity-frame)))
    (ng:treeview-heading treeview ng:+treeview-first-column-id+ :text "index")
    (ng:notebook-add activity-frame treeview :text (print-addr address))
    treeview))

(defun delete-current-tab (nb)
  (ng:format-wish "~a forget current" (ng:widget-path nb)))

(defun get-tab-text (nb)
  (ng:with-read-data ()
    ;; Note to time-travelling self:
    ;; - if you use "senddata" the result is interpreted as lisp code
    ;; - this causes the reader to freak out reading the colons in mac address
    ;; - dont waste 3 hours on a sunday, use "senddatastring"
    (ng:format-wish "senddatastring [~a tab current -text]" (ng:widget-path nb))
    ))

(defun select-latest-tab (nb)
  (let ((last-index (ng:with-read-data ()
                      (ng:format-wish "senddata [~a index end]" (ng:widget-path nb)))))
    (ng:format-wish "~a select ~a" (ng:widget-path nb) (if (zerop last-index) 0
                                                           (- last-index 1)))))

(defun make-connection-object (address-with-type handle treeview)
  (list :address-with-type address-with-type :handle handle :treeview treeview))

(defun handle->address (hci-handle connections)
  (maphash (lambda (address obj)
             (when (= (getf obj :handle) hci-handle)
               (return-from handle->address address)))
           connections))

;; From a real device (okay zephyr sample but still..)
(defparameter *sample-gatt*
  '((:HANDLE 1 :TYPE :SERVICE :END-HANDLE 8 :UUID 6145)
    (:HANDLE 2 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 32 :VALUE-HANDLE 3
     :UUID 10757)
    (:HANDLE 3 :TYPE :CHARACTERISTIC-VALUE :UUID 10757)
    (:HANDLE 4 :TYPE :CHARACTERISTIC-DESCRIPTOR :UUID 10498)
    (:HANDLE 5 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 10 :VALUE-HANDLE 6
     :UUID 11049)
    (:HANDLE 6 :TYPE :CHARACTERISTIC-VALUE :UUID 11049)
    (:HANDLE 7 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 2 :VALUE-HANDLE 8
     :UUID 11050)
    (:HANDLE 8 :TYPE :CHARACTERISTIC-VALUE :UUID 11050)
    (:HANDLE 9 :TYPE :SERVICE :END-HANDLE 15 :UUID 6144)
    (:HANDLE 10 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 2 :VALUE-HANDLE 11
     :UUID 10752)
    (:HANDLE 11 :TYPE :CHARACTERISTIC-VALUE :UUID 10752)
    (:HANDLE 12 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 2 :VALUE-HANDLE 13
     :UUID 10753)
    (:HANDLE 13 :TYPE :CHARACTERISTIC-VALUE :UUID 10753)
    (:HANDLE 14 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 2 :VALUE-HANDLE 15
     :UUID 10756)
    (:HANDLE 15 :TYPE :CHARACTERISTIC-VALUE :UUID 10756)
    (:HANDLE 16 :TYPE :SERVICE :END-HANDLE 23 :UUID 6157)
    (:HANDLE 17 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 16 :VALUE-HANDLE 18
     :UUID 10807)
    (:HANDLE 18 :TYPE :CHARACTERISTIC-VALUE :UUID 10807)
    (:HANDLE 19 :TYPE :CHARACTERISTIC-DESCRIPTOR :UUID 10498)
    (:HANDLE 20 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 2 :VALUE-HANDLE 21
     :UUID 10808)
    (:HANDLE 21 :TYPE :CHARACTERISTIC-VALUE :UUID 10808)
    (:HANDLE 22 :TYPE :CHARACTERISTIC-DECLARATION :PROPERTIES 8 :VALUE-HANDLE 23
     :UUID 10809)
    (:HANDLE 23 :TYPE :CHARACTERISTIC-VALUE :UUID 10809)))

(defun abbrev-attribute-type (attribute)
  (case attribute
    (:service "Service")
    (:characteristic-declaration " |  Char declaration")
    (:characteristic-value " |    | Char value")
    (:characteristic-descriptor " |    | Char descriptor")
    (otherwise " -- ")))

(defun format-attribute-data (attribute)
  ;; TODO: make this human-readable for svc, decl, desc
  (px (getf attribute :data)))

(defun add-gatt-table-to-treeview (treeview gatt-table)
  ;; Format of gatt-table is whatever lhost spits out
  (loop for attribute in gatt-table do
    (ng:treeview-insert-item
                  treeview
                  :id (princ-to-string (getf attribute :handle))
                  :text (format nil "~4,'0,X" (getf attribute :handle))
                  :column-values
                  (list
                   (abbrev-attribute-type (getf attribute :type))
                   (pretty-print-uuid (getf attribute :uuid))
                   (format-attribute-data attribute)))))

(defun add-gatt-table-to-conn (address gatt-table connections)
  (let* ((conn (gethash address connections)))
    (when conn
      (setf (getf (gethash address connections) :gatt-table) gatt-table)
      (add-gatt-table-to-treeview (getf conn :treeview) gatt-table))))

(defun get-selected-attribute (treeview)
  (when treeview
    (let ((sel (ng:treeview-get-selection treeview)))
      (when sel
        ;; item ID _is_ the handle
        (read-from-string (slot-value (car sel) 'ng:id))))))

(defparameter *cmds* (make-mailbox "ui backend -> host"))
(defparameter *evts* (make-mailbox "ui backend <- host"))

(defparameter *controller* (make-controller))

(defun do-rx-work (hci ui-events)
  ;; In the future, we'll have to extract data n stuff from here
  (drain-rxq hci)
  (loop
    (let ((packet (receive-rxq hci)))
      (queue ui-events packet)
      (if packet
          (process-hci hci (copy-tree packet))
          (return-from do-rx-work nil)))))

(defun encode-address (address)
  (list
   :address
   (subseq (make-c-int :u64 (getf address :address)) 0 6)
   :type (getf address :type)))

(defun start-backend (ui-events)
  (setf *controller* (make-controller))
  (list
   (start-hci
    (getf *controller* :tx-mailbox)
    (getf *controller* :rx-mailbox)
    (getf *controller* :stop-signal))
   (bt:make-thread
    (lambda ()
      (let ((hci *controller*)
            (*active-conns*)
            (*bonds* (make-hash-table))
            (bonds-stash))
        (loop
          (let ((cmd (sb-concurrency:receive-message *cmds* :timeout .1)))
            (when cmd
              (log-inf "[T] sending to host: ~X" cmd)
              (case (car cmd)
                (:init
                 (log-inf "INIT CONTROLLER")
                 (init-controller hci))

                (:start-scan
                 (progn
                   (log-inf "START SCAN")
                   (hci-set-scan-param hci)
                   (hci-set-scan-enable hci t)))
                (:stop-scan
                 (progn
                   (log-inf "STOP SCAN")
                   (hci-set-scan-enable hci nil)))

                (:connect
                 (let ((address (nth 1 cmd)))
                   (log-inf "CONNECT ~X" address)
                   (hci-set-scan-enable hci nil)
                   (hci-create-connection hci (encode-address address))))
                (:disconnect
                 (let ((conn-handle (nth 1 cmd)))
                   (log-inf "DISCONNECT")
                   (hci-disconnect hci conn-handle)))

                (:clear-security
                 (let ((conn-handle (nth 1 cmd))
                       (peer-address (nth 2 cmd)))
                   ;; Clear SMP context
                   (setf (get-smp-context conn-handle) '())

                   ;; SMP also reads *active-conns* for addresses
                   (setf (getf (getf *active-conns* conn-handle) :our-address)
                         (make-address (getf hci :random-address) #x01))
                   (setf (getf (getf *active-conns* conn-handle) :address)
                         peer-address)))
                (:bond
                 (let ((conn-handle (nth 1 cmd)))
                   (log-inf "BOND")
                   (start-security hci conn-handle)))
                (:stash-bonds
                 (progn
                   (log-inf "STASH BONDS")
                   (setf bonds-stash *bonds*)
                   (setf *bonds* (make-hash-table))))
                (:unstash-bonds
                 (progn
                   (log-inf "UN-STASH BONDS")
                   (when bonds-stash
                     (setf *bonds* bonds-stash)
                     (setf bonds-stash nil))))

                (:discover-gatt
                 (let* ((conn-handle (nth 1 cmd))
                        (gattc-table (gattc-discover hci conn-handle)))
                   (queue ui-events (list :gatt-discovery-end
                                          conn-handle
                                          gattc-table))))
                (:att-read
                 (log-inf "ATT READ"))
                (:att-write
                 (log-inf "ATT WRITE"))
                (:att-notify
                 (let* ((conn-handle (nth 1 cmd))
                        (handle (nth 2 cmd))
                        (data (nth 3 cmd)))
                   (log-inf "ATT NOTIFY C ~X H ~X D ~X" conn-handle handle data)
                   (notify hci conn-handle handle data)))
                (:quit
                 (progn
                   (log-inf "Exiting UI host interface")
                   (return nil)))))
            (do-rx-work hci ui-events)
            ))))
    :name "Host interface")))

(defun stop-backend (backend-thread)
  (ignore-errors
   (when backend-thread
     (queue *cmds* (list :quit))
     (stop-hci (getf *controller* :stop-signal))
     (bt:destroy-thread (nth 0 backend-thread))
     (bt:destroy-thread (nth 1 backend-thread))
     (sleep .5)
     (log-inf "KILLED ALL THREADS"))
   (loop while (drain-mailbox *evts*))
   (loop while (drain-mailbox *cmds*))

   ;; And a brand-new controller
   (setf *controller* (make-controller))
   ))

(defun dispatch-cmd (cmd-id &rest args)
  (log-inf "DISPATCH ~A~A" cmd-id (if args (format nil " ARGS ~X" args) ""))
  (queue *cmds* (push cmd-id args))
  t)

(defun fromhexstream (str &optional (bytes 1))
  (ignore-errors
   (with-input-from-string (is str)
     (loop for i from 0 below (length str) by 3
           collect
           (let ((parsed
                   (parse-integer (subseq str i (min (+ i 2) (length str))) :radix 16)))
             (when (< parsed (ash 1 (* 8 bytes)))
               parsed))))))

(defun cccd? (conn handle)
  (let ((peer-table (getf conn :gatt-table)))
    (when peer-table
      (eql (getf (nth (- handle 1) peer-table) :type) :characteristic-descriptor))))

(defun add-devices-to-treeview (treeview devices)
  (ng:treeview-delete-all treeview)
  (loop for device in devices do
    (ng:treeview-insert-item
     treeview
     :id (princ-to-string (getf device :address))
     :text (print-addr (getf device :address))
     :tag "tv"
     :column-values
     (list
      (princ-to-string (getf device :rssi))
      (getf device :name)
      (px (getf device :data))))))

(defun extract-addr-from-report (report)
  (make-address
   (getf report :address)
   (getf report :address-type)))

(setf *bonds* (make-hash-table))
(ng:with-nodgui ()
  (hci-log-reset)
  (ng:wm-title ng:*tk* "Azul '94")

  (let* ((ui-events (make-mailbox "ui events"))
         (content (make-instance 'ng:frame))
         (activity-frame (make-instance 'ng:notebook :master content
                                                     :height 400))
         (command-frame (make-instance 'ng:frame :master content
                                       ;; :borderwidth 5 :relief :ridge
                                       :width 200))
         (log-frame (make-instance 'ng:frame :master content))
         (log-textview (make-instance 'ng:scrolled-text :master log-frame
                                                        :state :disabled
                                                        :height 14))
         (*log-line-sink* (lambda (l) (log-to-widget log-textview l)))
         (treeview (make-instance 'ng:scrolled-treeview
                                  :columns (list "rssi" "name" "data")
                                  :master activity-frame))
         (menubar (ng:make-menubar))
         (scan-menu (make-instance 'ng:menu :master menubar :text "Scan"))
         (connection-menu (make-instance 'ng:menu :master menubar :text "Connection"))
         (att-menu (make-instance 'ng:menu :master menubar :text "GATT client"))
         (gatt-server-menu (make-instance 'ng:menu :master menubar :text "GATT server"))

         (connections (make-hash-table))
         (previous-gatt-notify-handle "0001")
         (previous-gatt-write "")
         (previous-gatt-notify "")
         (default-cccd-write "01 00")
         (backend-thread nil)
         (scanned-devices (make-hash-table))
         (sort-scan-by :rssi)
         (filter-scan-name "")
         (last-scan-display (get-internal-real-time))
         )

    ;; Register quit action
    (ng:on-close ng:*tk* (lambda () (queue ui-events :quit)))

    ;; Make text widget fill its frame
    (ng:grid-columnconfigure log-frame 0 :weight 1)

    ;; Add all the frames
    (ng:grid content 0 0 :sticky "nsew")
    (ng:grid activity-frame 0 1 :sticky "nsew")
    (ng:grid command-frame 0 0 :sticky "nsew")
    (ng:grid log-frame 1 0 :columnspan 2 :sticky "nsew")
    (ng:grid log-textview 0 0 :sticky "nsew")

    (ng:notebook-add activity-frame treeview :text "Scanner")

    ;; Autosize root window
    (ng:grid-columnconfigure ng:*tk* 0 :weight 1)
    (ng:grid-rowconfigure ng:*tk* 0 :weight 1)
    (ng:resizable ng:*tk* 0 0)

    ;; Populate the command palette
    (let ((row -1))
      (mapcar
       (lambda (button)
         (ng:grid button (incf row) 0 :sticky "ew" :padx 5 :pady 2))
       (list
        (make-button command-frame "Start scan"
                     (lambda () (queue ui-events :start-scan)))
        (make-button command-frame "Stop scan"
                     (lambda () (queue ui-events :stop-scan)))
        (make-button command-frame "Connect"
                     (lambda () (queue ui-events :connect)))
        (make-button command-frame "Disconnect"
                     (lambda () (queue ui-events :disconnect)))
        (make-button command-frame "Advertise"
                     (lambda () (queue ui-events :start-adv)))
        (make-button command-frame "Stop adv"
                     (lambda () (queue ui-events :stop-adv)))
        )))

    ;; Populate the activity view.
    ;; Set sort functions on column label click
    (labels ((sort-column (by)
               (setf sort-scan-by by)
               (queue ui-events :display-scanned-devices)))

      (ng:treeview-heading treeview ng:+treeview-first-column-id+
                           :text "MAC"
                           :command (lambda () (sort-column :recent)))
      (ng:treeview-heading treeview "rssi"
                           :command (lambda () (sort-column :rssi)))
      (ng:treeview-heading treeview "name"
                           :command (lambda () (sort-column :name)))

      ;; Autosize column widths
      ;; TODO: fixed column sizes please
      (ng:treeview-refit-columns-width treeview))

    ;; connection view
    ;; - show perms per attribute
    ;;
    ;;MVP: adv and gatts can be a text file
    ;; - advertising data editor
    ;; - gatt server editor
    ;;
    ;; - encryption
    ;;   - delete bonds (backup before)
    ;;   - save/restore bonds (save to app dir)
    ;;
    ;; scan filters:
    ;; - have manuf-data (ie garmin)
    ;; - have uuids
    ;; - connectable
    ;; - name glob
    ;;
    ;; misc
    ;; - gatt table clone
    ;; - mitm +view
    ;; - serial port path
    ;; - serial port open/close buttons
    ;; - copy data fields to clipboard
    ;;
    ;; Host hookup
    ;; - advertising start/stop
    ;; - gatt read/write
    ;; - implement cancel-connection (disconnect button on Scanner view)

    ;; Scan menu
    (make-menuitem scan-menu "Scan" ui-events :start-scan "<s>")
    (make-menuitem scan-menu "Stop scan" ui-events :stop-scan "<S>")
    (make-menuitem scan-menu "Set scan name filter" ui-events :filter-scan "<f>")
    (make-menuitem scan-menu "Connect" ui-events :connect "<c>")

    ;; In-connection menu
    (make-menuitem connection-menu "Disconnect" ui-events :disconnect "<d>")
    (make-menuitem connection-menu "Update connection params" ui-events :update-conn-params "<u>" :disabled)
    (make-menuitem connection-menu "Exchange MTU" ui-events :exchange-mtu "<m>" :disabled)

    ;; Security
    (make-menuitem connection-menu "Bond" ui-events :bond "<b>")
    (make-menuitem connection-menu "Encrypt (no bonding)" ui-events :encrypt "<e>" :disabled)
    (make-menuitem connection-menu "Stash bonds" ui-events :stash-bonds "<Control-b>")
    (make-menuitem connection-menu "Unstash bonds" ui-events :unstash-bonds "<Control-B>")

    ;; GATT client
    (make-menuitem att-menu "Read" ui-events :att-read "<r>" :disabled)
    (make-menuitem att-menu "Write" ui-events :att-write "<w>" :disabled)

    ;; GATT server
    (make-menuitem gatt-server-menu "Read" ui-events :gatt-server-get "<R>" :disabled)
    (make-menuitem gatt-server-menu "Write" ui-events :gatt-server-set "<W>" :disabled)
    (make-menuitem gatt-server-menu "Notify" ui-events :att-notify "<N>" :disabled)
    (make-menuitem gatt-server-menu "Clone peer table" ui-events :gatt-server-clone "<Control-C>" :disabled)

    ;; Big business
    ;; TODO: move that to explicit user action. e.g. START button.
    (stop-backend nil)
    (sleep .1)

    (setf backend-thread (start-backend ui-events))
    (dispatch-cmd :init)

    ;; Print out GATT table
    ;; TODO: make a proper pane for this
    (log-inf "GATT TABLE: ~A" (gattc-print *gatts-table*))

    ;; Poll for events
    (loop while
      (let ((evt (sb-concurrency:receive-message ui-events)))
        (unless (listp evt)
          (log-dbg "UI-EVT: ~A" evt))
        (case (if (listp evt) (car evt) evt)
          (:start-scan
           (dispatch-cmd evt))
          (:stop-scan
           (dispatch-cmd evt))
          (:filter-scan
           (progn
             (setf filter-scan-name
                   (nodgui.mw:text-input-dialog
                    content "Scan filter" "Enter name to filter by"
                    :text filter-scan-name))
             (log-inf "Set scan filter to: ~A" filter-scan-name)
             t))

          (:connect
           (let ((device (get-selected-device treeview)))
             (when (and device
                        (not (gethash (getf device :address) connections)))
               (let* ((reports (getf (gethash (getf device :address) scanned-devices) :reports))
                      (address-with-type (extract-addr-from-report (nth 0 reports))))
                 (when address-with-type
                   (dispatch-cmd :connect address-with-type))))
             t))
          (:disconnect
           (let ((name (get-tab-text activity-frame)))
             (unless (equalp "Scanner" name)
               (ignore-errors
                (let ((handle (getf (gethash (decode-mac name) connections) :handle)))
                  (log-inf "Disconnecting ~a" name)
                  (remhash (decode-mac name) connections)
                  (delete-current-tab activity-frame)
                  (dispatch-cmd :disconnect handle))))
             t))

          (:bond
           (let ((name (get-tab-text activity-frame)))
             (unless (equalp "Scanner" name)
               (ignore-errors
                (let ((handle (getf (gethash (decode-mac name) connections) :handle)))
                  (log-inf "Bonding ~a" name)
                  (dispatch-cmd :bond handle))))
             t))

          (:stash-bonds
           (progn
             (dispatch-cmd :stash-bonds)
             t))

          (:unstash-bonds
           (progn
             (dispatch-cmd :unstash-bonds)
             t))

          (:att-read
           (let* ((tab-name (get-tab-text activity-frame))
                  (conn (gethash (decode-mac tab-name) connections))
                  (handle (get-selected-attribute (getf conn :treeview))))
             (when handle
               (log-inf "[~A] att-read ~4,'0,X" tab-name handle)
               (dispatch-cmd :att-read (getf conn :address) handle))
             t))
          (:att-write
           (let* ((tab-name (get-tab-text activity-frame))
                  (conn (gethash (decode-mac tab-name) connections))
                  (handle (get-selected-attribute (getf conn :treeview))))
             (when handle
               (log-inf "[~A] att-write ~4,'0,X" tab-name handle)
               (let* ((cccd (cccd? conn handle))
                      (data
                        (nodgui.mw:text-input-dialog
                         content "GATT Write" "GATT Write Data (e,g, 01 ef 32 c1)"
                         :text (if cccd default-cccd-write previous-gatt-write)))
                      (parsed-data (fromhexstream data)))
                 (when parsed-data
                   (unless cccd (setf previous-gatt-write data))
                   (dispatch-cmd :att-write (getf conn :address) handle data))))
             t))
          (:att-notify
           ;; TODO: log writes/reads to UI window too
           (let* ((tab-name (get-tab-text activity-frame))
                  (conn-handle (getf (gethash (decode-mac tab-name) connections) :handle)))
             (when conn-handle
               (let* ((handle
                        (nodgui.mw:text-input-dialog
                         content "Handle" "GATT Notify handle (hex)"
                         :text previous-gatt-notify-handle))
                      (parsed-handle (decode-c-int (fromhexstream handle 2) :u16))
                      (data
                        (nodgui.mw:text-input-dialog
                         content "GATT Notify Data" "GATT Notify Data (e,g, 01 ef 32 c1)"
                         :text previous-gatt-notify))
                      (parsed-data (fromhexstream data)))
                 (when (and parsed-handle parsed-data)
                   (log-inf "[~A] att-notify ~4,'0,X" tab-name handle)
                   (setf previous-gatt-notify data)
                   (setf previous-gatt-notify-handle handle)
                   (dispatch-cmd :att-notify conn-handle parsed-handle parsed-data))))
             t))

          (:start-adv t)
          (:stop-adv t)
          (:quit
           (progn
             ;; TODO: rename this. confusing.
             (stop-backend backend-thread)
             nil))

          (:display-scanned-devices
           (progn
             (add-devices-to-treeview
              treeview
              (sort-scan sort-scan-by
                         (filter-scan filter-scan-name
                                      (make-device-list scanned-devices))))
             t))

          (:gatt-discovery-end
           (let* ((conn-handle (nth 1 evt))
                  (table (nth 2 evt))
                  (address (handle->address conn-handle connections)))
             (add-gatt-table-to-conn address table connections)
             t))

          ;; HCI event
          (:evt
           (progn
             (case (car (cadr evt))
               (:le-scan-report
                (progn
                  (accumulate-scan-reports scanned-devices (cadr evt))
                  (when (> (- (get-internal-real-time) last-scan-display) 10000)
                    (setf last-scan-display (get-internal-real-time))
                    (queue ui-events :display-scanned-devices))))

               (:le-enh-conn-complete
                (let* ((data (cadr (cadr evt)))
                       (conn-handle (getf data :handle))
                       (address-with-type (make-address
                                           (decode-c-int (getf data :peer-address) :u64)
                                           (getf data :peer-address-type)))
                       (address (getf address-with-type :address)))

                  ;; Build treeview, store it along with address and handle
                  (setf
                   (gethash address connections)
                   (make-connection-object
                    address-with-type conn-handle (make-connection-tab activity-frame address)))

                  ;; Focus new treeview
                  (select-latest-tab activity-frame)

                  ;; Reset SMP context
                  (dispatch-cmd :clear-security conn-handle address-with-type)

                  ;; Kick off GATT discovery
                  (dispatch-cmd :discover-gatt conn-handle)
                  ))

               (:encryption-change
                (let* ((data (cadr (cadr evt)))
                       (conn-handle (getf data :handle)))

                  ;; TODO: ui element showing we're bonded
                  (log-inf "[~X] BONDED OK" (handle->address conn-handle connections))
                  ))

               (:otherwise
                (log-inf "EVENT: ~X" evt)))
           t))

          (:acl
           (let ((att-packet (decode-att (cadr evt))))
             (when att-packet
               (log-inf "ATT RX: ~X" att-packet)
               ;; TODO: highlight CCCD writes
               ;; TODO: print LTK to UI window too
               )
             t))

          (otherwise (log-err "UNHANDLED EVENT ~X" evt))
          )))

    (setf *test* scanned-devices)
    (log-inf "Exiting UI")
    (hci-log-write)
    (ng:exit-nodgui)
    ))
