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

(get-scanned-devices *devices*)

(defun make-button (frame text command)
  (make-instance 'ng:button
                 :master frame
                 :text text
                 :command command))

(ng:with-nodgui ()
  (ng:wm-title ng:*tk* "grid-example.lisp")
                                        ; first, make some widgets and parent frames
  (let* ((content (make-instance 'ng:frame))
         (activity-frame (make-instance 'ng:frame :master content
                                                  :borderwidth 5 :relief :ridge
                                                  :width 600 :height 400))
         (command-frame (make-instance 'ng:frame :master content
                                                 ;; :borderwidth 5 :relief :ridge
                                                 :width 200))
         (log-frame (make-instance 'ng:frame :master content
                                             :width 600 :height 200))
         )

    ;; Add all the frames
    (ng:grid content 0 0 :sticky "nsew")
    (ng:grid activity-frame 0 1 :sticky "nsew")
    (ng:grid command-frame 0 0 :sticky "nsew")
    (ng:grid log-frame 1 0 :columnspan 2 :sticky "nsew")

    ;; autosize root window
    (ng:grid-columnconfigure ng:*tk* 0 :weight 1)
    (ng:grid-rowconfigure ng:*tk* 0 :weight 1)
    (ng:resizable ng:*tk* 0 0)

    ;; Populate the command palette
    (let ((row -1)
          (buttons
            (list
             (make-button command-frame "Start scan"
                          (lambda () (log-inf "start-scan")))
             (make-button command-frame "Stop scan"
                          (lambda () (log-inf "stop-scan")))
             (make-button command-frame "Connect"
                          (lambda () (log-inf "connect")))
             (make-button command-frame "Disconnect"
                          (lambda () (log-inf "disconnect")))
             (make-button command-frame "Advertise"
                          (lambda () (log-inf "start-adv")))
             (make-button command-frame "Stop adv"
                          (lambda () (log-inf "stop-adv")))
             )))

      (loop for button in buttons do
        (ng:grid button (incf row) 0 :sticky "ew" :padx 5 :pady 2)))
      ))
