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
                              :data (append
                                     (make-ad :flags '(#x01))
                                     (make-ad-name "Nose Thermometer 800"))))
   (list #xF89DC52A5202 (list :rssi -69
                              :data (append
                                     (make-ad :flags '(#x01))
                                     (make-ad-name "Delorean audio"))))
   (list #xF89DC52A5202 (list :rssi -80
                              :data (append
                                     (make-ad :flags '(#x01))
                                     (make-ad-name "overwrites"))))
   (list #xF89DD2215203 (list :rssi -42
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
    (when encoded-name
      (from-c-string encoded-name))))

;; Returns a list of address + rssi + name + data
(defun get-scanned-devices (devices &key (order-by :rssi))
  (declare (ignore order-by))
  (let ((devs))
    (maphash (lambda (address details)
               (push (list :address address
                           :name (parse-name (getf details :data))
                           :rssi (getf details :rssi)
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

(ng:with-nodgui ()
  (ng:wm-title ng:*tk* "azul")
                                        ; first, make some widgets and parent frames
  (let* ((ui-events (make-mailbox "ui events"))
         (content (make-instance 'ng:frame))
         (activity-frame (make-instance 'ng:frame :master content
                                                  :borderwidth 5 :relief :ridge
                                                  :width 600 :height 400))
         (command-frame (make-instance 'ng:frame :master content
                                       ;; :borderwidth 5 :relief :ridge
                                       :width 200))
         (log-frame (make-instance 'ng:frame :master content
                                             :width 600 :height 200))
         (treeview (make-instance 'ng:scrolled-treeview
                                  :columns (list "rssi" "name" "data")
                                  :master activity-frame))
         )

    ;; Register quit action
    (ng:on-close ng:*tk* (lambda () (queue ui-events :quit)))

    ;; Add all the frames
    (ng:grid content 0 0 :sticky "nsew")
    (ng:grid activity-frame 0 1 :sticky "nsew")
    (ng:grid command-frame 0 0 :sticky "nsew")
    (ng:grid log-frame 1 0 :columnspan 2 :sticky "nsew")

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
    ;; For now, this is just a list of scanned devices.
    ;; Set sort functions on column label click
    (ng:treeview-heading treeview ng:+treeview-first-column-id+
                         :text "MAC"
                         :command (lambda () (log-err "Sorting by last packet")))
    (ng:treeview-heading treeview "rssi"
                         :command (lambda () (log-err "Sorting by RSSI")))
    (ng:treeview-heading treeview "name"
                         :command (lambda () (log-err "Sorting by name")))
    (ng:grid treeview 0 0 :sticky "nsew")

    ;; Add devices
    (loop for device in (get-scanned-devices *devices*) do
      (ng:treeview-insert-item treeview
                               :id (princ-to-string (getf device :address))
                               :text (print-addr (getf device :address))
                               :tag "tv"
                               :column-values
                               (list
                                (princ-to-string (getf device :rssi))
                                (getf device :name)
                                (px (getf device :data)))))

    ;; Autosize column widths
    ;; TODO: fixed column sizes please
    (ng:treeview-refit-columns-width treeview)

    ;; TODO:
    ;; - spawn a thread for RX HCI events
    ;; - give it the 'ui-events mailbox
    ;; - return that TID so we can terminate it when we quit
    ;; - that thread puts host events on the mailbox
    ;; - we handle them just like UI events

    ;; Poll for events
    (loop while
      (let ((evt (sb-concurrency:receive-message ui-events)))
        (log-dbg (format nil "EVT: ~A" evt))
        (ecase evt
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
