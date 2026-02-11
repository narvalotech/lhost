;; eval host.lisp before
(ql:quickload "nodgui")
;; (nodgui.demo:demo)
(setf nodgui:*default-theme* "default")
(sb-ext:add-package-local-nickname :ng :nodgui)

;; SCAN MVP:
;;
;; - two panes:
;;   - button palette on left side
;;   - scan results on the right side
;;
;; for now:
;; - use synthetic scan results
;;
;; real data:
;; - start scan
;; - receive-if :scan-rsp
;;   - collect all scan-rsp by address
;;   - have summarizing fn to merge all into a single entry
;;
;; scan result (ie device) is
;; - address
;; - last rssi
;; - last name
;; - merged data
;;
;; filters:
;; - have manuf-data (ie garmin)
;; - have uuids
;; - connectable
(defparameter *devices* (make-hash-table))
(defparameter *testlist*
  (list
   (list #xF89DC52A5C01 (list :rssi -128
                              :timestamp 0
                              :data (append
                                     (make-ad :flags '(#x01))
                                     (make-ad-name "Nose Thermometer 800"))))
   (list #xF89DC52A5202 (list :rssi -69
                              :timestamp 12312
                              :data (append
                                     (make-ad :flags '(#x01))
                                     (make-ad-name "Delorean audio"))))
   (list #xF89DC52A5202 (list :rssi -80
                              :timestamp 12319
                              :data (append
                                     (make-ad :flags '(#x01))
                                     (make-ad-name "overwrites"))))
   (list #xF89DD2215203 (list :rssi -42
                              :timestamp 10319
                              :data (append
                                     (make-ad :flags '(#x01))
                                     (make-ad-name "HeartRate Sensor 100"))))))
(mapcar (lambda (el)
          (setf (gethash (car el) *devices*)
                (cadr el))) *testlist*)

(defun make-a-sign (byte)
  (if (logbitp 7 byte)
      (- byte (ash 1 8))
      byte))

(defun decode-scan-report (evt)
  ;; TODO: handle multiple reports
  (let ((report (nth 0 (getf (nth 1 evt) :reports))))
    (list :type (getf report :address-type)
          :address (decode-c-int (getf report :address))
          :timestamp 0                  ; later dude
          :rssi (make-a-sign (getf report :rssi))
          :data (getf report :data))))

(defun accumulate-scan-reports (dest report)
  ;; Track the last 10 reports for each address
  (let ((decoded (decode-scan-report report)))
    (push decoded (gethash (getf decoded :address) dest))
    (nbutlast (gethash (getf decoded :address) dest) 10)
    nil))

(defun extract-name (ad)
  (ignore-errors                        ; boooo
   (from-c-string
    (getf (parse-ad ad) :name-complete))))

(defun make-device-list (devices-dict)
  (let ((devices))
    (maphash (lambda (address reports)
               (declare (ignore address))
               (let* ((report (nth 0 reports))
                      (name (extract-name (getf report :data))))
                 (push (append report (list :name name)) devices)))
             devices-dict)
    devices))

(defun parse-name (ad)
  (let* ((parsed (parse-ad ad))
         (encoded-name (or (getf parsed :name-complete)
                           (getf parsed :name-short))))
    (if encoded-name
        (from-c-string encoded-name)
        "")))

;; Returns a list of address + rssi + name + data
(defun get-scanned-devices (devices &key (order-by :rssi))
  (declare (ignore order-by))
  (let ((devs))
    (maphash (lambda (address details)
               (push (list :address address
                           :name (parse-name (getf details :data))
                           :rssi (getf details :rssi)
                           :timestamp (getf details :timestamp)
                           :data (getf details :data))
                     devs))
             devices)
    devs))

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

(get-scanned-devices *devices*)
 ; => ((:ADDRESS 273356718952963 :NAME "HeartRate Sensor 100" :RSSI -42 :DATA
 ;  (2 1 1 21 9 72 101 97 114 116 82 97 116 101 32 83 101 110 115 111 114 32 49
 ;   48 48))
 ; (:ADDRESS 273356501438978 :NAME "overwrites" :RSSI -80 :DATA
 ;  (2 1 1 11 9 111 118 101 114 119 114 105 116 101 115))
 ; (:ADDRESS 273356501441537 :NAME "Nose Thermometer 800" :RSSI -128 :DATA
 ;  (2 1 1 21 9 78 111 115 101 32 84 104 101 114 109 111 109 101 116 101 114 32
 ;   56 48 48)))

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

(defun make-menuitem (master text q command-id &optional accelerator)
  (apply #'make-instance
         (append
          (list 'ng:menubutton :master master :text text
                               :command (lambda () (queue q command-id)))
          (when accelerator
            ;; Bind the keyboard event
            (ng:bind (ng:root-toplevel) accelerator (lambda (e)
                                                      (declare (ignore e))
                                                      (queue q command-id)))
            ;; Display the keyboard shortcut in the menu
            (list
             :accelerator accelerator)))))

(defun sort-scan (devices by)
  (sort devices
        (case by
          (:recent (lambda (a b) (< (getf a :timestamp) (getf b :timestamp))))
          (:rssi (lambda (a b) (> (getf a :rssi) (getf b :rssi))))
          (:name (lambda (a b) (string-lessp (getf a :name) (getf b :name))))
          (otherwise (lambda (a b) (string-lessp (getf a :name) (getf b :name)))))))

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

(defun make-connection-object (address treeview)
  (list :address address :treeview treeview))

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

(defun format-uuid (uuid)
  ;; TODO: pretty-print 128-bit uuid
  (format nil "~X" uuid))

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
                   (format-uuid (getf attribute :uuid))
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
          (process-hci hci packet)
          (return-from do-rx-work nil)))))

(defun start-backend (ui-events)
  (setf *controller* (make-controller))
  (list
   (start-hci
    (getf *controller* :tx-mailbox)
    (getf *controller* :rx-mailbox)
    (getf *controller* :stop-signal))
   (bt:make-thread
    (lambda ()
      (let ((hci *controller*))
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
                   (hci-set-scan-enable hci t)
                   ))

                (:stop-scan
                 (progn
                   (log-inf "STOP SCAN")
                   (hci-set-scan-enable hci nil)
                   ))

                (:att-read
                 (log-inf "ATT READ"))
                (:att-write
                 (log-inf "ATT WRITE"))
                (:quit
                 (progn
                   (log-inf "Exiting UI backend")
                   (return nil)))))
            (do-rx-work hci ui-events)
            ))))
    :name "UI backend <=> Host")))

(defun stop-backend (backend-thread)
  (ignore-errors
   (when backend-thread
     (queue *cmds* (list :quit))
     (stop-hci (getf *controller* :stop-signal))
     (bt:destroy-thread (nth 0 backend-thread))
     (bt:destroy-thread (nth 1 backend-thread))
     (sleep .5)
     )
   (loop while (drain-mailbox *evts*))
   (loop while (drain-mailbox *cmds*))

   ;; And a brand-new controller
   (setf *controller* (make-controller))
   ))

(defun dispatch-cmd (cmd-id &rest args)
  (log-inf "DISPATCH ~A ARGS ~X" cmd-id args)
  (queue *cmds* (push cmd-id args))
  t)

(defun fromhexstream (str)
  (ignore-errors
   (with-input-from-string (is str)
     (loop for i from 0 below (length str) by 3
           collect
           (let ((parsed
                   (parse-integer (subseq str i (min (+ i 2) (length str))) :radix 16)))
             (when (< parsed #x100)
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

(ng:with-nodgui ()
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
         (previous-gatt-write "")
         (default-cccd-write "01 00")
         (backend-thread nil)
         (scanned-devices (make-hash-table))
         (sort-scan-by :rssi)
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
               (add-devices-to-treeview treeview
                (sort-scan (make-device-list scanned-devices) sort-scan-by))))

      (ng:treeview-heading treeview ng:+treeview-first-column-id+
                           :text "MAC"
                           :command (lambda () (sort-column :recent)))
      (ng:treeview-heading treeview "rssi"
                           :command (lambda () (sort-column :rssi)))
      (ng:treeview-heading treeview "name"
                           :command (lambda () (sort-column :name)))

      ;; Add devices
      (add-devices-to-treeview treeview (get-scanned-devices *devices*))

      ;; Autosize column widths
      ;; TODO: fixed column sizes please
      (ng:treeview-refit-columns-width treeview))

    ;; TODO:
    ;; - spawn a thread for RX HCI events
    ;; - give it the 'ui-events mailbox
    ;; - return that TID so we can terminate it when we quit
    ;; - that thread puts host events on the mailbox
    ;; - we handle them just like UI events

    ;; Remaining work
    ;; - use real cmds/events
    ;;
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
    ;; misc
    ;; - gatt table clone
    ;; - mitm +view

    ;; Scan menu
    (make-menuitem scan-menu "Scan" ui-events :start-scan "<s>")
    (make-menuitem scan-menu "Stop scan" ui-events :stop-scan "<S>")
    (make-menuitem scan-menu "Connect" ui-events :connect "<c>")

    ;; In-connection menu
    (make-menuitem connection-menu "Disconnect" ui-events :disconnect "<d>")
    (make-menuitem connection-menu "Encrypt (no bonding)" ui-events :encrypt "<e>")

    (make-menuitem connection-menu "Bond" ui-events :bond "<b>")
    (make-menuitem connection-menu "Update connection params" ui-events :update-conn-params "<u>")
    (make-menuitem connection-menu "Exchange MTU" ui-events :exchange-mtu "<m>")

    ;; GATT client
    (make-menuitem att-menu "Read" ui-events :att-read "<r>")
    (make-menuitem att-menu "Write" ui-events :att-write "<w>")

    ;; GATT server
    (make-menuitem gatt-server-menu "Read" ui-events :gatt-server-get "<R>")
    (make-menuitem gatt-server-menu "Write" ui-events :gatt-server-set "<W>")
    (make-menuitem gatt-server-menu "Notify" ui-events :att-notify "<N>")
    (make-menuitem gatt-server-menu "Clone peer table" ui-events :gatt-server-clone)

    ;; Big business
    ;; TODO: move that to explicit user action. e.g. START button.
    (stop-backend nil)
    (sleep .1)

    (setf backend-thread (start-backend ui-events))
    (dispatch-cmd :init)

    ;; Poll for events
    ;; TODO: dispatch host commands in another thread
    (loop while
      (let ((evt (sb-concurrency:receive-message ui-events)))
        (unless (listp evt)
          (log-dbg "EVT: ~A" evt))
        (case (if (listp evt) (car evt) evt)
          (:start-scan
           (dispatch-cmd evt))
          (:stop-scan
           (dispatch-cmd evt))
          (:connect
           (let ((device (get-selected-device treeview)))
             (when (and device
                        (not (gethash (getf device :address) connections)))
               (log-inf "connect to ~A" device)

               ;; Build treeview and store it along with address in the connection list
               (setf
                (gethash (getf device :address) connections)
                (make-connection-object
                 (getf device :address)
                 (make-connection-tab activity-frame (getf device :address))))
               ;; Focus new treeview
               (select-latest-tab activity-frame)

               ;; Add a dummy GATT table to test out UI
               (add-gatt-table-to-conn
                (getf device :address) *sample-gatt* connections))
             t))
          (:disconnect
           (let ((name (get-tab-text activity-frame)))
             (unless (equalp "Scanner" name)
               (ignore-errors
                (remhash (decode-mac name) connections))
               (log-inf "Disconnecting ~a" name)
               (delete-current-tab activity-frame))
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

          (:start-adv t)
          (:stop-adv t)
          (:quit
           (progn
             ;; TODO: rename this. confusing.
             (stop-backend (cadr backend-thread))
             nil))

          ;; HCI event
          (:evt
           (progn
             (case (car (cadr evt))
               (:le-scan-report
                (progn
                  (accumulate-scan-reports scanned-devices (cadr evt))
                  ;; maybe rebuild on a timer instead?
                  (add-devices-to-treeview
                   treeview
                   (sort-scan
                    (make-device-list scanned-devices)
                    sort-scan-by)
                  )))
               (:otherwise
                (log-inf "EVENT: ~X" evt)))
           t))
          )))

    (log-inf "Exiting UI")
    (ng:exit-nodgui)
    ))
