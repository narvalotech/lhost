;; eval host.lisp before
(ql:quickload "nodgui")
(setf nodgui:*default-theme* "yaru")
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
       (slot-value (car sel) 'ng:id)
       :values
       (slot-value (car sel) 'ng:column-values)))))

(defun queue (mailbox event)
  (sb-concurrency:send-message mailbox event))

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
  (ng:configure textview :state :disabled))

(ng:with-nodgui ()
  (ng:wm-title ng:*tk* "azul")

  (let* ((ui-events (make-mailbox "ui events"))
         (content (make-instance 'ng:frame))
         (activity-frame (make-instance 'ng:frame :master content
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
    (labels ((add-devices-to-treeview (devices)
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
             (sort-column (by)
               (add-devices-to-treeview
                (sort-scan (get-scanned-devices *devices*) by))))

      (ng:treeview-heading treeview ng:+treeview-first-column-id+
                           :text "MAC"
                           :command (lambda () (sort-column :recent)))
      (ng:treeview-heading treeview "rssi"
                           :command (lambda () (sort-column :rssi)))
      (ng:treeview-heading treeview "name"
                           :command (lambda () (sort-column :name)))
      (ng:grid treeview 0 0 :sticky "nsew")

      ;; Add devices
      (add-devices-to-treeview (get-scanned-devices *devices*))

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
    ;; - add device/connection view
    ;;   - figure out how to show a device (GATT treeview?)
    ;;   - render a sample gatt table (from lhost)
    ;; - use real events
    ;;
    ;; connection view
    ;; - gatt read
    ;; - gatt write
    ;; - gatt subscribe
    ;; - gatt server editor
    ;; - gatt table clone
    ;; - encryption
    ;;
    ;; misc
    ;; - mitm +view

    ;; Scan menu
    (make-menuitem scan-menu "Scan" ui-events :start-scan "<Control-s>")
    (make-menuitem scan-menu "Stop scan" ui-events :stop-scan "<Control-S>")
    (make-menuitem scan-menu "Connect" ui-events :connect "<Control-c>")

    ;; In-connection menu
    (make-menuitem connection-menu "Disconnect" ui-events :disconnect "<Control-d>")
    (make-menuitem connection-menu "Encrypt (no bonding)" ui-events :encrypt "<Control-e>")

    (make-menuitem connection-menu "Bond" ui-events :bond "<Control-b>")
    (make-menuitem connection-menu "Update connection params" ui-events :update-conn-params "<Control-u>")
    (make-menuitem connection-menu "Exchange MTU" ui-events :exchange-mtu "<Control-m>")

    ;; GATT client
    (make-menuitem att-menu "Read" ui-events :att-read "<Control-r>")
    (make-menuitem att-menu "Write" ui-events :att-write "<Control-w>")
    (make-menuitem att-menu "Subscribe" ui-events :att-sub "<Control-x>")

    ;; GATT server
    (make-menuitem gatt-server-menu "Read" ui-events :gatt-server-get "<Control-R>")
    (make-menuitem gatt-server-menu "Write" ui-events :gatt-server-set "<Control-W>")
    (make-menuitem gatt-server-menu "Notify" ui-events :att-notify "<Control-N>")
    (make-menuitem gatt-server-menu "Clone peer table" ui-events :gatt-server-clone)

    ;; Poll for events
    ;; TODO: move this to another thread
    (loop while
      (let ((evt (sb-concurrency:receive-message ui-events)))
        (log-dbg (format nil "EVT: ~A" evt))
        (case evt
          (:start-scan t)
          (:stop-scan t)
          (:connect
           (progn
             (log-inf
              (format nil "connect to ~A"
                      (get-selected-device treeview)))
             t))
          (:disconnect t)
          (:start-adv t)
          (:stop-adv t)
          (:quit nil))))

    (log-inf "Exiting UI")
    (ng:exit-nodgui)
    ))
