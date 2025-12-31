(declaim (optimize (debug 3)))

(require 'sb-posix)

;;;;;;;;;;;;; general utils

(defconstant +u64-max+ (ldb (byte 64 0) -1))

(defun make-range (max)
  (loop for number from 0 to (- max 1) collect number))

(defun extract-byte (number index)
  (ldb (byte 8 (* index 8)) number))

(defun make-uint (octets number)
  (loop for pos in (make-range octets)
        collect (extract-byte number pos)))

(make-uint 4 8000)
 ; => (64 31 0 0)

(defun u2b (type)
  (case type
    (:u8 1)
    (:u16 2)
    (:u32 4)
    (:u64 8)))

(defun custom-type (type)
  (case type
    (:bt-addr 6)))

(defun type->octets (type)
  (let ((octets))
    (setf octets (u2b type))

    (if (not octets)
        (setf octets (custom-type type)))

    (if (not octets)
        (error "unknown type ~A" type))

    octets))

(type->octets :u32)
 ; => 4 (3 bits, #x4, #o4, #b100)
(type->octets :bt-addr)
 ; => 6 (3 bits, #x6, #o6, #b110)

(defun make-c-int (type value)
  (make-uint (type->octets type) value))

(make-c-int :u8 #xFF)
 ; => (255)
(make-c-int :u32 8000)
 ; => (64 31 0 0)

(defun decode-c-int (bytes &optional type)
  (let ((result 0)
        (data (if type
                  (subseq bytes 0 (u2b type))
                  bytes)))
    (dolist (byte (reverse data) result) ; Reverse the byte order for little endian
      (setf result (logior (ash result 8) byte)))))

(decode-c-int '(1 2 3 4))
 ; => 67305985 (27 bits, #x4030201)
(decode-c-int '(1 2 3 4) :u16)
 ; => 513 (10 bits, #x201)

(defun make-c-struct-member (member)
  ;; name is first el, unused here
  (let ((type (nth 1 member))
        (value (nth 2 member)))
    (make-c-int type value)))

(defun make-c-struct (members)
  ;; Make a c-struct. `members' is a list of (name type value)
  ;; Output is a list of bytes.
  (mapcan #'make-c-struct-member members))

;; Eg. write default data length cmd. Len = 200 bytes, time = 1000 us
(make-c-struct '(("tx-octets" :u16 200) ("tx-time-us" :u16 1000)))
 ; => (200 0 232 3)

;;;;;;;;;;;;; babblesim PHY

(defconstant PB_MSG_WAIT #x01)
(defconstant PB_MSG_WAIT_END #x81)
(defconstant PB_MSG_TERMINATE #xFFFE)
(defconstant PB_MSG_DISCONNECT #xFFFF)

(defun make-wait-cmd (us)
  (append
   (make-uint 4 PB_MSG_WAIT)
   (make-uint 8 us)))

(make-wait-cmd 10)

(defun make-terminate-cmd ()
  (make-uint 4 PB_MSG_TERMINATE))

(defun read-bytes (bytes stream)
  (let ((b
          (if (equal bytes 1)
              (list (read-byte stream))
              (loop for byte from 0 to (- bytes 1) collect
                                                   (read-byte stream)))))
    ;; (format t "read-bytes: ~A~%" b)
    b))

(defun make-simplex-raw-fd-stream (file is-output)
  (sb-sys:make-fd-stream (sb-posix:file-descriptor file)
                         :element-type 'unsigned-byte
                         :input (not is-output)
                         :output is-output
                         :buffering :none))

(defun open-simplex-fd (path is-output)
  (let ((file (if is-output
                  (open path :direction :output :if-exists :overwrite)
                  (open path :direction :input))))
    (make-simplex-raw-fd-stream file is-output)))

(defun sim-wait (ms sim)
  (let ((rx (getf sim :rx))
        (tx (getf sim :tx))
        (time (getf sim :time)))
    (setf time (+ time (* ms 1000)))
    (format t "waiting until ~A us (delta ~A ms).." (getf sim :time) ms)

    (write-sequence (make-wait-cmd time) tx)
    (setf (getf sim :time) time)

    (read-bytes 4 rx)
    (format t "done~%")))

(defun sim-terminate (sim)
  (format t "term~%")
  (write-sequence (make-terminate-cmd) (getf sim :tx)))

;;;;;;;;;;;;; HCI packet-building

(defconstant +h4-types+
  (list :cmd #x1
        :acl #x2
        :evt #x4
        :iso #x5))

(defun plist-key (plist value)
  (loop for (key val) on plist by #'cddr
        when (equal val value)
          return key))

(defun make-h4 (type payload)
  (append
   (list (getf +h4-types+ type))
   payload))

;; Schema is (:name (#xOPCODE PARAM-PLIST RSP-PLIST))
(defparameter *hci-cmds*
  '(:reset (#x0c03 nil (:status :u8))

    :write-default-data-length
    (#x2024
     (:tx-octets :u16
      :tx-time-us :u16)
     nil)

    :set-event-mask
    (#x0c01 (:events :u64) (:status :u8))

    :le-set-event-mask
    (#x2001 (:events :u64) (:status :u8))

    :set-random-address
    (#x2005 (:address :bt-addr) (:status :u8))

    :set-adv-param
    (#x2006 (:min-interval :u16
             :max-interval :u16
             :type :u8
             :own-address-type :u8
             :peer-address-type :u8
             :peer-address :bt-addr
             :channel-map :u8
             :filter-policy :u8)
     (:status :u8))

    :set-adv-data
    (#x2008 (:len :u8
             :data (list :u8))
     (:status :u8))

    :set-adv-enable
    (#x200a (:enable :u8) (:status :u8))

    :set-scan-param
    (#x200B (:type :u8
             :interval :u16
             :window :u16
             :own-address-type :u8
             :filter-policy :u8)
     (:status :u8))

    :set-scan-enable
    (#x200C (:enable :u8
             :filter-duplicates :u8)
     (:status :u8))

    :create-connection
    (#x200D (:scan-interval :u16
             :scan-window :u16
             :filter-policy :u8
             :peer-address-type :u8
             :peer-address (list :u8)
             :own-address-type :u8
             :interval-min :u16
             :interval-max :u16
             :max-latency :u16
             :supervision-timeout :u16
             :min-connection-event-length :u16
             :max-connection-event-length :u16)
     nil)

    :disconnect
    (#x0406 (:handle :u16
             :reason :u8)
     nil)

    :read-buffer-size
    (#x2002
     nil
     (:status :u8
      :le-len :u16
      :le-num :u8))
    ))

(getf *hci-cmds* :write-default-data-length)
 ; => (8228 (:TX-OCTETS :U16 :TX-TIME-US :U16) NIL)
(getf *hci-cmds* :read-buffer-size)
 ; => (8194 NIL (:STATUS :U8 :LE-LEN :U16 :LE-NUM :U8))

;; Destructively pop `amount` bytes out of `buffer` which is a list of bytes.
(defmacro pull (buffer amount)
  `(let ((bytes (subseq ,buffer 0 ,amount)))
     (setf ,buffer (subseq ,buffer ,amount))
     bytes))

(defmacro pull-int (buffer type)
  `(decode-c-int (pull ,buffer (u2b ,type))))

(defparameter *test* '(#x4 #x1 #x3 #xc #x0))
(pull-int *test* :u16)
 ; => 260 (9 bits, #x104)
(pull-int *test* :u16)
 ; => 3075 (12 bits, #xC03)

(defun opcode->cmd (opcode)
  (loop for (command properties) on *hci-cmds* by #'cddr
        when (equal (nth 0 properties) opcode)
          return command))

(opcode->cmd #x2002)
 ; => :READ-BUFFER-SIZE

(defun command-properties (opcode)
  (loop for (command properties) on *hci-cmds* by #'cddr
        when (equal (nth 0 properties) opcode)
          return (list command properties)))

(command-properties #x2002)
 ; => (:READ-BUFFER-SIZE (8194 NIL (:STATUS :U8 :LE-LEN :U16 :LE-NUM :U8)))

(defun parse-cmd-response (opcode payload)
  (let* ((command (command-properties opcode))
         (properties (nth 1 command))
         (schema (if properties (nth 2 properties))))

    (when t
      ;; loop over param plist, replacing
      (loop for (name type) on schema by #'cddr
            nconc
            (list name (pull-int payload type))))))

(parse-cmd-response #x2002 '(0 #xFB 0 3))
 ; => (:STATUS 0 :LE-LEN 251 :LE-NUM 3)

(defun evt-cmd-complete (payload)
  (let ((ncmd (pull-int payload :u8))
        (opcode (pull-int payload :u16)))
    (list :cmd-complete
          (list
           :ncmd ncmd
           :opcode opcode
           :params (parse-cmd-response opcode payload)))))

(evt-cmd-complete '(#x1 #x3 #xc #x0))
 ; => (:CMD-COMPLETE (:NCMD 1 :OPCODE 3075 :PARAMS (:STATUS 0)))
(evt-cmd-complete '(1 2 #x20 0 #xFB 0 3))
 ; => (:CMD-COMPLETE (:NCMD 1 :OPCODE 8194 :PARAMS (:STATUS 0 :LE-LEN 251 :LE-NUM 3)))

(defun evt-cmd-status (payload)
  (list
   :cmd-status
   (list
    :status (pull-int payload :u8)
    :ncmd (pull-int payload :u8)
    :opcode (pull-int payload :u16))))

(evt-cmd-status '(#x1 #x1 #x3 #xc))
 ; => (:CMD-STATUS (:STATUS 1 :NCMD 1 :OPCODE 3075))

(defun decode-adv-report (payload)
  (let* ((num-reports (pull-int payload :u8))
         (reports
           (loop for i from 0 to (- num-reports 1)
                 collect
                 (let* ((event-type (pull-int payload :u8))
                        (address-type (pull-int payload :u8))
                        (address (pull payload 6))
                        (data-length (pull-int payload :u8))
                        (data (pull payload data-length))
                        (rssi (pull-int payload :u8))) ; i8 technically
                   (list
                    :event-type event-type
                    :address-type address-type
                    :address address
                    :data-length data-length
                    :data data
                    :rssi rssi)))))
    (list :le-scan-report
          (list :num-reports num-reports
                :reports reports))))

(defun decode-conn-complete (payload)
  (list
   :le-conn-complete
   (list
    :status (pull-int payload :u8)
    :handle (pull-int payload :u16)
    :role (pull-int payload :u8)
    :peer-address-type (pull-int payload :u8)
    :peer-address (pull payload 6)
    :interval (pull-int payload :u16)
    :latency (pull-int payload :u16)
    :timeout (pull-int payload :u16)
    :clock-accuracy (pull-int payload :u8))))

(defun decode-enh-conn-complete (payload)
  (list
   :le-enh-conn-complete
   (list
    :status (pull-int payload :u8)
    :handle (pull-int payload :u16)
    :role (pull-int payload :u8)
    :peer-address-type (pull-int payload :u8)
    :peer-address (pull payload 6)
    :local-rpa (pull payload 6)
    :peer-rpa (pull payload 6)
    :interval (pull-int payload :u16)
    :latency (pull-int payload :u16)
    :timeout (pull-int payload :u16)
    :clock-accuracy (pull-int payload :u8))))

(defun decode-channel-selection-algo (payload)
  (list :le-channel-selection-algo
        :handle (pull-int payload :u16)
        :algo (pull-int payload :u8)))

(defun decode-le-meta (payload)
  (let ((sub (pull-int payload :u8)))
    (case sub
      (#x02 (decode-adv-report payload))
      (#x01 (decode-conn-complete payload))
      (#x0A (decode-enh-conn-complete payload))
      (#x14 (decode-channel-selection-algo payload))
      (otherwise (list :le-unknown :sub sub :raw payload)))))

(defun evt-le-meta (payload)
  (decode-le-meta payload))

(defun evt-disc-complete (payload)
  (list :disconnection-complete
        (list :status (pull-int payload :u8)
              :handle (pull-int payload :u16)
              :reason (pull-int payload :u8))))

(defun evt-num-completed-packets (payload)
  (let ((parsed
          (list :number-of-completed-packets
                (list :num-handles (pull-int payload :u8)
                      :handle (pull-int payload :u16)
                      :num (pull-int payload :u16)))))
    (when (> (getf (cadr parsed) :num-handles) 1)
      (error "Got NCP with >1 conn handle."))
    parsed))

(defparameter *hci-events*
  '(#x05 evt-disc-complete
    #x0e evt-cmd-complete
    #x0f evt-cmd-status
    #x13 evt-num-completed-packets
    #x3e evt-le-meta))

(getf *hci-events* #x0e)
 ; => EVT-CMD-COMPLETE

(funcall (getf *hci-events* #x0e) '(#x1 #x3 #xc #x0))
 ; => (:CMD-COMPLETE (:NCMD 1 :OPCODE 3075 :PARAMS (0)))

(defun decode-hci-event (header payload)
  (let* ((opcode (pull-int header :u8))
         (len (pull-int header :u8))
         (handler (getf *hci-events* opcode)))
    (declare (ignore len))

    (if (not handler) (error "No entry for op ~X [payload ~A]" opcode payload))

    (funcall handler payload)))

(format nil "~x" (decode-hci-event '(#x0e #x04) '(#x1 #x3 #xc #x0)))
 ; => "(CMD-COMPLETE (NCMD 1 OPCODE C03 PARAMS (STATUS 0)))"
(format nil "~x" (decode-hci-event '(#xe #x7) '(1 2 #x20 0 #xFB 0 3)))
 ; => "(CMD-COMPLETE (NCMD 1 OPCODE 2002 PARAMS (STATUS 0 LE-LEN FB LE-NUM 3)))"

(defun make-hci-cmd-param (name value spec)
  (let ((type (getf spec name)))

    (unless type
      (error (format nil "Unknown param: ~A" name)))

    ;; If it's a list, assume it's a "raw" list of bytes.
    ;; If it's a type specifier (e.g. :u8), interpret it as a "c struct member".
    (if (listp type)
        value
        (make-c-struct-member (list name type value)))))

;; test it
(make-hci-cmd-param
 :tx-time-us 1000
 (nth 1 (getf *hci-cmds* :write-default-data-length)))
 ; => (232 3)
(getf *hci-cmds* :write-default-data-length)
 ; => (8228 (:TX-OCTETS :U16 :TX-TIME-US :U16) NIL)

(defun serialize-hci-params (params spec)
  (when params
    ;; loop over param plist
    (loop for (name value) on params by #'cddr
          nconc
          (make-hci-cmd-param name value spec))))

(defun make-hci-cmd (cmd-name &rest params)
  (let* ((spec (getf *hci-cmds* cmd-name))
         (opcode (car spec))
         (param-spec (nth 1 spec))
         (serialized-params
           (serialize-hci-params params param-spec)))

    ;; (format t "OP: ~x param-spec: ~A params ~A~%" opcode param-spec params)
    (append
     (make-c-int :u16 opcode)
     (make-c-int :u8 (length serialized-params))
     serialized-params)))

;; HCI cmds in plist
;; name: (plist of :param-name :type)
(format nil "~A~%" (make-hci-cmd :write-default-data-length
                                 :tx-octets 200
                                 :tx-time-us 1000))
; cmd WRITE-DEFAULT-DATA-LENGTH op 2024 param-spec: (TX-OCTETS U16 TX-TIME-US U16) params (TX-OCTETS
;                                                                                          C8
;                                                                                          TX-TIME-US
;                                                                                          3E8)
;  => "(36 32 4 200 0 232 3)
; "

(defun send (hci type payload)
  "Format a payload into H4 and send to hci device"
  (let ((stream (getf hci :h2c))
        (packet (make-h4 type payload)))
    (format t "TX: ~x~%" packet)
    (write-sequence packet stream)))

(defun h4-parse-opcode (packet)
  "Looks up the H4 opcode"
  (plist-key +h4-types+ (car packet)))

(defun hci-header-len (opcode)
  "Returns the length of the HCI packet header field"
  (case opcode
    (:evt 2)
    (:acl 4)
    (:iso 4)
    (t (error "doesn't look like anything to me"))))

(defun hci-header-len-field (opcode)
  "Returns the offset and the size of the length field"
  (case opcode
    (:evt '(1 1))
    (:acl '(2 2))
    (:iso '(2 2))
    (t (error "doesn't look like anything to me"))))

(defun hci-parse-len (opcode packet)
  "Extracts the payload length from the HCI packet header"
  (let* ((header (hci-header-len-field opcode))
         (offset (nth 0 header))
         (size (nth 1 header)))
    (decode-c-int
     (subseq packet
             ;; skip the H4 opcode/header byte
             (+ 1 offset) (+ 1 offset size)))))

(hci-parse-len :acl '(2 1 1 0 0 ))
(hci-parse-len :evt '(#x4 #xe #x4 #x1 #x3 #xc #x0))

(defun rx-h4 (stream)
  (let ((packet '())
        (opcode)
        (header)
        (payload))

    ;; TODO: desync handling
    ;; TODO: don't re-iterate

    ;; read h4 opcode
    ;; (format t "read op~%")
    (setf packet (read-bytes 1 stream))
    ;; (format t "packet: ~A~%" packet)

    ;; parse h4 opcode
    (setf opcode (h4-parse-opcode packet))

    ;; read HCI packet header
    ;; (format t "read header~%")
    (setf header (read-bytes (hci-header-len opcode) stream))
    (setf packet (append packet header))
    ;; (format t "header: ~X~%" header)
    ;; (format t "packet: ~A~%" packet)

    ;; parse hci-packet-length from header & read payload
    ;; (format t "read payload~%")
    (setf payload (read-bytes
                   (hci-parse-len opcode packet)
                   stream))
    (setf packet (append packet payload))
    (format t "RX[H4]: ~X~%" packet)

    ;; return raw packet and parsed packet
    (list
     :opcode opcode
     :header header
     :payload payload
     :raw packet)))

(defun append-to-acl-in (hci fragment)
  (setf (getf hci :acl-in)
        (append (getf hci :acl-in) fragment)))

(defun decode-l2cap (conn fragment)
  (list :conn-handle conn
        :length (pull-int fragment :u16)
        :channel (pull-int fragment :u16)
        :data fragment))

(defconstant +l2-hdr-size+ 4)

(defun complete-acl (hci conn fragment)
  ;; for now only one conn supported
  (let* ((l2pac (append-to-acl-in hci fragment))
         (l2size
           (+ +l2-hdr-size+
              (decode-c-int (subseq l2pac 0 2))))
         (current-size (length l2pac)))
    ;; TODO: error handling?
    (format t "[ACL-APPEND] (~A/~A) ~X~%" current-size l2size l2pac)
    (if (= l2size current-size)
        (progn
          (setf (getf hci :acl-in) '())
          (decode-l2cap conn l2pac))
        nil)))

(defun decode-acl-type (header)
  (ldb (byte 2 12) (logand (decode-c-int header) #xB000)))

(defun decode-hci-acl (hci header payload)
  (let ((conn-handle (logand (decode-c-int header) #x0FFF))
        (pb-flag (decode-acl-type header)))
    (if (zerop pb-flag)
        ;; This branch is dead code with the current controller rn
        (decode-l2cap conn-handle payload)
        (complete-acl hci conn-handle payload))))

(defun receive (hci)
  "Receive and decode a single HCI packet"
  ;; initial implementation is H4
  (let ((stream (getf hci :c2h)))
    (let* ((packet (rx-h4 stream))
           (opcode (getf packet :opcode))
           (header (getf packet :header))
           (payload (getf packet :payload)))

      (case opcode
        (:evt (list :evt (decode-hci-event header payload)))
        (:acl (list :acl (decode-hci-acl hci header payload)))
        (t (error "doesn't look like anything to me"))))))

(defun add-to-rxq (hci packet)
  (when (cadr packet)
    (push packet (getf hci :rxq))))

(defun receive-rxq (hci)
  (pop (getf hci :rxq)))

(defun receive-if (hci predicate &key (from-list nil))
  ;; TODO: add timeout maybe? But what about bsim blocking process?
  (loop
    (let ((packet (if from-list (receive-rxq hci) (receive hci))))
      (if (funcall predicate packet)
          ;; strip the H4 header
          (return-from receive-if (cadr packet))
          (add-to-rxq hci packet)))))

(defun receive-cmd (hci)
  "Wait for the next command response/status"
  (receive-if
   hci
   (lambda (packet)
     ;; TODO: also check for command-id
     (progn
       ;; (break)
       (and (eql (car packet) :evt)
            (or (eql (car (cadr packet)) :cmd-status)
                (eql (car (cadr packet)) :cmd-complete)))))))

;;;;;;;;;;;;; host

(defun make-hci-dev (h2c-stream c2h-stream)
  (list
   :h2c h2c-stream
   :c2h c2h-stream
   :rxq '()
   :acl-in '()
   :acl-tx-size 0
   :acl-tx-num 0
   :acl-rx-size 0
   :random-address 0))

;;;;;;;;;;;;; script

;; Run the REPL
(defparameter *bs-rx-path* "/tmp/bs_jon/myid/2G4.d0.ptd")
(defparameter *bs-tx-path* "/tmp/bs_jon/myid/2G4.d0.dtp")
(defparameter *h2c-path*   "/tmp/lhost/uart.h2c")
(defparameter *c2h-path*   "/tmp/lhost/uart.c2h")

(defparameter sizes '(:acl-tx-size 0
                      :acl-rx-size 1))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro with-bsim (instance rx-path tx-path &body body)
  (with-gensyms (rx tx)
    `(with-open-stream (,rx (open-simplex-fd ,rx-path nil))
       (with-open-stream (,tx (open-simplex-fd ,tx-path t))
         (let ((,instance (list :rx ,rx :tx ,tx :time 0)))
           (progn ,@body)
           )))))

(defmacro with-hci (instance h2c-path c2h-path &body body)
  (with-gensyms (h2c c2h)
    `(with-open-stream (,h2c (open-simplex-fd ,h2c-path t))
       (with-open-stream (,c2h (open-simplex-fd ,c2h-path nil))
         (let ((,instance (make-hci-dev ,h2c ,c2h)))
           (progn ,@body)
           )))))

;; (with-bsim sim *bs-rx-path* *bs-tx-path*
;;   (format t "connected to PHY (rx ~A tx ~A)~%"
;;           (sb-posix:file-descriptor (getf sim :rx))
;;           (sb-posix:file-descriptor (getf sim :tx)))
;;   (sim-wait 1000 sim)
;;   (sim-terminate sim))

(defun hci-send-cmd (hci cmd)
  "Send a command and check it's return status. Return status/params if no error."

  (send hci :cmd cmd)
  (let ((response (receive-cmd hci)))
    ;; Here `response` is an HCI event object, e.g.
    ;; (CMD-COMPLETE (NCMD 1 OPCODE C03 PARAMS (STATUS 0)))
    (format t "RX: ~x~%" response)

    (if (eql (car response) :cmd-status)
        (let* ((status (getf (nth 1 response) :status)))
          (unless (zerop status)
            (format t "cmd failed: status 0x~x~%" status)
            (break))
          status)

        ;; cmd-complete
        (let* ((data (nth 1 response))
               (params (getf data :params))
               (status (getf params :status)))

          (if (not (equal status 0))
              (progn
                (format t "cmd failed: status 0x~x~%" status)
                (break)))

          (if (equal status 0)
              params
              nil)))))

(defun hci-reset (hci)
  "Reset the controller"
  (hci-send-cmd hci (make-hci-cmd :reset)))

(defun hci-read-buffer-size (hci)
  "Read (and set) H->C buffer lengths and amount"
  (let ((params
          (hci-send-cmd hci (make-hci-cmd :read-buffer-size))))

    (if params
        (let ((le-len (getf params :le-len))
              (le-num (getf params :le-num)))

          (setf (getf hci :acl-tx-size) le-len)
          (setf (getf hci :acl-tx-num) le-num)
          t))))

(defun hci-allow-all-the-events (hci)
  "Allow controller to send us all the possible events"
  (hci-send-cmd hci (make-hci-cmd :set-event-mask
                                  :events +u64-max+))
  (hci-send-cmd hci (make-hci-cmd :le-set-event-mask
                                  :events +u64-max+)))

(defun hci-set-random-address (hci address)
  (let ((status (hci-send-cmd hci (make-hci-cmd :set-random-address
                                                :address address))))
    (if status
        (setf (getf hci :random-address) address))))

(defun hci-set-adv-param (hci)
  ;; We hardcode connectable advertising to get started
  (hci-send-cmd
   hci
   (make-hci-cmd :set-adv-param
                 :min-interval 60
                 :max-interval 60
                 ;; connectable, scannable, undirected
                 :type #x00
                 ;; random address
                 :own-address-type #x01
                 ;; no directed, no care
                 :peer-address-type 0
                 :peer-address 0
                 ;; use all channels
                 :channel-map #b111
                 ;; #nofilter
                 :filter-policy 0)))

(defun hci-set-adv-enable (enable hci)
  (hci-send-cmd hci (make-hci-cmd :set-adv-enable :enable (if enable 1 0))))

(defun hci-set-adv-data (hci data)
  "Sets advertising data. Input is a list of AD structures (byte lists)."
  (let ((flattened (mapcan #'append data)))
    (hci-send-cmd
     hci
     (make-hci-cmd :set-adv-data
                   :len (length flattened) :data flattened))))

(defun hci-set-scan-param (hci)
  (hci-send-cmd
   hci
   (make-hci-cmd :set-scan-param
                 :type #x01             ; active scan
                 :interval 60
                 :window 60
                 :own-address-type #x01
                 :filter-policy 0)))

(defun hci-set-scan-enable (hci enable)
  (hci-send-cmd
   hci
   (make-hci-cmd :set-scan-enable
                 ;; active
                 :enable (if enable #x01 #x00)
                 :filter-duplicates #x00)))

(defun hci-create-connection (hci address)
  (hci-send-cmd
   hci
   (make-hci-cmd :create-connection
                 :scan-interval 60
                 :scan-window 60
                 :filter-policy 0
                 :peer-address-type (getf address :type)
                 :peer-address (getf address :address)
                 :own-address-type #x01 ; random
                 :interval-min 100
                 :interval-max 100
                 :max-latency 0
                 :supervision-timeout 200
                 :min-connection-event-length 0
                 :max-connection-event-length #xffff)))

;; TODO: define all error codes
(defconstant +remote-user-terminated+ #x13)

(defun hci-disconnect (hci handle)
  (hci-send-cmd
   hci
   (make-hci-cmd :disconnect
                 :handle handle
                 :reason +remote-user-terminated+)))

(defconstant +ad-types+
  (list :flags #x01
        :class-uuid-16-incomplete #x02
        :class-uuid-16-complete #x03
        :class-uuid-32-incomplete #x04
        :class-uuid-32-complete #x05
        :class-uuid-128-incomplete #x06
        :class-uuid-128-complete #x07

        :sollicitation-uuid-16 #x14
        :sollicitation-uuid-32 #x1f
        :sollicitation-uuid-128 #x15

        :data-uuid-16 #x16
        :data-uuid-32 #x20
        :data-uuid-128 #x21

        :name-short #x08
        :name-complete #x09
        :tx-power #x0a
        :class #x0d
        :device-id #x10 ; duplicate w/ :sm-tk-value in the spec?

        :sm-tk-value #x10
        :sm-oob-flags #x11
        :pairing-hash-c-192 #x0e
        :pairing-randomizer-r-192 #x0f
        :pairing-hash-c-256 #x1d
        :pairing-randomizer-r-256 #x1e
        :lesc-confirmation-value #x22
        :lesc-random-value #x23

        :peripheral-connection-interval-range #x12
        :public-target-address #x17
        :random-target-address #x18
        :appearance #x19
        :advertising-interval #x1a
        :le-device-address #x1b
        :le-role #x1c

        :uri #x24
        :indoor-positioning #x25
        :transport-discovery-data #x26
        :le-supported-features #x27
        :channel-map-update-indication #x28

        :pb-adv #x29
        :mesh-message #x2a
        :mesh-beacon #x2b

        :big-info #x2c
        :broadcast-code #x2d
        :broadcast-name #x30
        :past-information #x32

        :resolvable-set-indentifier #x2e
        :advertising-interval-long #x2f
        :encrypted-ad #x31
        :electronic-shelf-label #x34
        :3d-information-data #x3d
        :manufacturer-specific #xff))

(defun make-ad (type payload)
  "Makes an AD (advertising data) struct. type is number/specifier, payload is byte list."
  ;; accepts both number and name for type
  ;; e.g. :encrypted-ad or #x31
  (if (>= (length payload) (- 256 8))
      (error "AD payload is too big"))

  (append (list
           (+ 1 (length payload))
           (if (numberp type)
               type
               (getf +ad-types+ type)))
          payload))

(make-ad :flags '(#x01))
 ; => (2 1 1)
(make-ad #x01 '(#x03))
 ; => (2 1 3)

(defun char->utf8 (char)
  "Converts a CL character to a UTF-8 (list of bytes)."
  ;; Phind made this! 10 bucks well spent
  ;; https://www.phind.com/search?cache=kohb0esnotediegmqr58rt1m
  (let ((code (char-code char)))
    (cond
      ;; 1-byte sequence
      ((<= code #x7F)
       (list code))
      ;; 2-byte sequence
      ((<= code #x7FF)
       (list (logior #xC0 (ash code -6))
             (logior #x80 (logand code #x3F))))
      ;; 3-byte sequence
      ((<= code #xFFFF)
       (list (logior #xE0 (ash code -12))
             (logior #x80 (logand (ash code -6) #x3F))
             (logior #x80 (logand code #x3F))))
      ;; 4-byte sequence
      ((<= code #x10FFFF)
       (list (logior #xF0 (ash code -18))
             (logior #x80 (logand (ash code -12) #x3F))
             (logior #x80 (logand (ash code -6) #x3F))
             (logior #x80 (logand code #x3F)))))))

(char->utf8 #\a)
 ; => (97)
(char->utf8 #\λ)
 ; => (206 187)
(char->utf8 #\💻)
 ; => (240 159 146 187)

(defun to-c-string (cl-string &optional null-terminated)
  "Converts a CL string to a list of bytes"
  (append
   (mapcan (lambda (c) (char->utf8 c)) (coerce cl-string 'list))
   (if null-terminated (list 0) nil)))

(to-c-string "test")
 ; => (116 101 115 116)
(to-c-string "test" t)
 ; => (116 101 115 116 0)
(to-c-string "🔵-🦷")
 ; => (240 159 148 181 45 240 159 166 183)

(defun from-c-string (bytes)
  "Decode an ASCII-encoded string from a list of bytes"
  ;; This one doesn't support UTF-8
  (with-output-to-string (s)
    (dolist (b bytes)
      (write-char (code-char b) s))))

(defun make-ad-name (name)
  (make-ad :name-complete (to-c-string name)))

(make-ad-name "hello")
 ; => (6 9 104 101 108 108 111)
(make-ad-name "🎉")
 ; => (5 9 240 159 142 137)

(defun evt? (evt-code)
  (lambda (packet)
    (eql (car (cadr packet)) evt-code)))

(defun wait-for-scan-report (hci predicate)
  ;; TODO: handle multiple reports
  (let ((evt (receive-if hci (evt? :le-scan-report))))
    (let ((report (find-if predicate (getf (nth 1 evt) :reports))))
      (when report
        (list :type (getf report :address-type)
              :address (getf report :address))))))

(defun wait-for-conn (hci)
  (let ((evt (receive-if hci (evt? :le-enh-conn-complete))))
    (getf (nth 1 evt) :handle)))

(defun wait-for-disconn (hci)
  (let ((evt (receive-if hci (evt? :disconnection-complete))))
    (getf (nth 1 evt) :handle)))

(defun ncp? (conn-handle)
  (lambda (p)
    (and (eql (car p) :evt)
         (eql (car (cadr p)) :number-of-completed-packets)
         (eql (getf (cadr (cadr p)) :handle) conn-handle))))

(defun wait-for-ncp (hci handle)
  (let ((queued (receive-if hci (ncp? handle) :from-list t)))
    (if queued
        queued
        (receive-if hci (ncp? handle)))))

;; Mandatory:
;; - error-rsp
;; - find-information-req
;; - read-req
(defconstant +att-errors+
  (list :invalid-handle #x01
        :read-not-permitted #x02
        :write-not-permitted #x03
        :invalid-pdu #x04
        :insufficient-authentication #x05
        :request-not-supported #x06
        :invalid-offset #x07
        :insufficient-authorization #x08
        :prepare-queue-full #x09
        :attribute-not-found #x0A
        :attribute-not-long #x0B
        :encryption-key-size-too-short #x0C
        :invalid-attribute-value-length #x0D
        :unlikely-error #x0E            ; zephyr host be like..
        :insufficient-encryption #x0F
        :unsupported-group-type #x10
        :insufficient-resources #x11
        :database-out-of-sync #x12
        :value-not-allowed #x13
        :application-error-start #x80
        :application-error-end #x9F
        :common-profile-and-service-errors-start #xE0
        :common-profile-and-service-errors-end #xFF))

(defconstant +att-opcodes+
  (list :error-rsp #x01
        :exchange-mtu-req #x02
        :exchange-mtu-rsp #x03
        :find-information-req #x04
        :find-information-rsp #x05
        :find-by-type-value-req #x06
        :find-by-type-value-rsp #x07
        :read-by-type-req #x08
        :read-by-type-rsp #x09
        :read-req #x0A
        :read-rsp #x0B
        :read-blob-req #x0C
        :read-blob-rsp #x0D
        :read-multiple-req #x0E
        :read-multiple-rsp #x0F
        :read-by-group-type-req #x10
        :read-by-group-type-rsp #x11
        :write-req #x12
        :write-rsp #x13
        :write-cmd #x52
        :prepare-write-req #x16
        :prepare-write-rsp #x17
        :execute-write-req #x18
        :execute-write-rsp #x19
        :read-multiple-variable-req #x20
        :read-multiple-variable-rsp #x21
        :multiple-handle-value-ntf #x23
        :handle-value-ntf #x1B
        :handle-value-ind #x1D
        :handle-value-cfm #x1E
        :signed-write-cmd #xD2))

(defun att-make-opcode (op-name &optional single)
  (if (not single)
      (make-c-int :u8 (getf +att-opcodes+ op-name))
      (getf +att-opcodes+ op-name)))

(defconstant +att-requests+
  (mapcar (lambda (o) (att-make-opcode o t))
          (list
           :exchange-mtu-req
           :find-information-req
           :find-by-type-value-req
           :read-by-type-req
           :read-req
           :read-blob-req
           :read-multiple-req
           :read-by-group-type-req
           :write-req
           :write-cmd
           :prepare-write-req
           :execute-write-req
           :read-multiple-variable-req
           :signed-write-cmd)))

(defun att-make-packet (op param)
  ;; TODO: check param is MTU-1
  (append (att-make-opcode op) param))

(defun hci-send-acl (hci conn-handle packet)
  (send hci
        :acl
        (append
         (make-c-int :u16 conn-handle)
         (make-c-int :u16 (length packet))
         packet)))

(defun l2cap-send (hci conn-handle channel packet)
  (hci-send-acl
   hci
   conn-handle
   (append
    (make-c-int :u16 (length packet))
    (make-c-int :u16 channel)
    packet)))

(defconstant +l2cap-att-chan+ #x0004)

(defun att? (conn-handle &optional opcode-value req)
  (lambda (p)
    ;; sample p: (ACL (CONN-HANDLE 0 LENGTH 3 CHANNEL 4 DATA (3 40 1)))
    (and (eql (car p) :acl)
         (eql (getf (cadr p) :conn-handle)
              conn-handle)
         (eql (getf (cadr p) :channel)
              +l2cap-att-chan+)
         (if opcode-value
             (or (eql (car (getf (cadr p) :data)) (car opcode-value))
                 (eql (car (getf (cadr p) :data)) (att-make-opcode :error-rsp t)))
             t)
         (if req
             (member (car (getf (cadr p) :data))
                     +att-requests+)
             t))))

(defun att-receive (hci conn-handle opcode)
  (receive-if hci (att? conn-handle (att-make-opcode opcode))))

(defun att-send (hci conn-handle payload)
  (l2cap-send hci conn-handle +l2cap-att-chan+ payload))

(defun att-set-mtu (hci conn-handle mtu)
  ;; Send client RX MTU
  (att-send
   hci
   conn-handle
   (att-make-packet :exchange-mtu-req
                    (make-c-int :u16 mtu)))
  ;; Response is server RX MTU
  (let ((rsp (att-receive hci conn-handle :exchange-mtu-rsp)))
    (when rsp
      (pull-int (getf rsp :data) :u8)     ; opcode
      (pull-int (getf rsp :data) :u16)))) ; server RX MTU

;; Async design:
;;
;; send cmd: send immediately
;; -> how to wait for response?
;; -> start HCI-RAW-RX with filter
;; -> when filter doesn't match, push RX packet to HCI-RX queue
;; -> when filter matches, return
;;
;; send data: send immediately
;; -> same pattern
;; -> be wary of data re-ordering
;;
;; idle loop:
;; -> pull from HCI-RX, dispatch events/data
;; -> pull from HCI-RAW-RX, dispatch directly
;;
;; TODO
;; - [x] add packet filtering
;; - [x] add packet queues
;; - [x] decode ATT packets
;; - [] add NCP / TX queues
;; - [] add processing of queues?
;; - [x] acl (RX) fragmentation
;;
;; GATT Client
;; - [x] error pdu
;; - [x] find-information
;; - [x] discovery
;; - [x] read/write
;; - [x] subscribe (CCCD)
;;
;; GATT Server
;; - [] error pdu
;; - [] find-information
;; - [] read/write
;; - [] notifications
;;
;; SMP
;; - [] periph security request
;; - [] JustWorks pairing

(defun decode-handles-and-uuids (data &key 128-bit)
  (loop while data collecting
        (if 128-bit
            (list :handle (pull-int data :u16)
                  :uuid-128 (pull data 16))
            (list :handle (pull-int data :u16)
                  :uuid-16 (pull-int data :u16)))))

(defun att-decode-find-information-rsp (data)
  (let ((128-bit (= 2 (pull-int data :u8))))
    (decode-handles-and-uuids data :128-bit 128-bit)))

(defun att-find-information (hci conn-handle
                             &optional (start 1) (end #xFFFF))
  (unless (>= end start)
    (error "Bad handle range [~A:~A]" start end))
  (att-send hci conn-handle
            (att-make-packet :find-information-req
                             (append
                              (make-c-int :u16 start)
                              (make-c-int :u16 end))))
  (let* ((rsp (att-receive hci conn-handle :find-information-rsp))
         (data (getf rsp :data)))
    (when rsp
      (unless (att-error? (pull-int data :u8))
        (att-decode-find-information-rsp data)))))

(defun att-decode-read-by-group-type-rsp (data)
  ;; All the attributes *must* have the same data length.
  ;; We need to send another read-by-group for other lengths.
  ;; who designed this smh
  (let ((el-len (- (pull-int data :u8) 4)))
    (loop while data
          collecting
          (list :handle (pull-int data :u16)
                :end-handle (pull-int data :u16)
                :value (pull data el-len)))))

(defun att-error? (opcode)
  (eql opcode (att-make-opcode :error-rsp t)))

(defun att-read-by-group-type (hci conn-handle uuid
                               &optional (start 1) (end #xFFFF))
  (unless (> end start)
    (error "Bad handle range [~A:~A]" start end))
  (att-send hci conn-handle
            (att-make-packet :read-by-group-type-req
                             (append
                              (make-c-int :u16 start)
                              (make-c-int :u16 end)
                              uuid)))
  (let* ((rsp (att-receive hci conn-handle :read-by-group-type-rsp))
         (data (getf rsp :data)))
    (when rsp
      (unless (att-error? (pull-int data :u8))
        (att-decode-read-by-group-type-rsp data)))))

(defconstant +gatt-uuid-primary-service+ #x2800)
(defconstant +gatt-uuid-characteristic+ #x2803)
(defconstant +gatt-uuid-cccd+ #x2902)
(defconstant +gatt-uuid-heart-rate-service+ #x180D)
(defconstant +gatt-uuid-heart-rate-measurement+ #x2A37)
(defconstant +gatt-uuid-gap-device-name+ #x2A00)

(defun encode-uuid (uuid)
  ;; TODO: 128-bit
  (make-c-int :u16 uuid))

(defun last-handle (rsp)
  (getf (car (last rsp)) :end-handle))

(defun discover-services (hci conn)
  ;; Discover all (primary) services
  ;; [v5.4 - p1489]
  ;;
  ;; response: (handle group-handle value)
  ;; handle: Handle of service declaration
  ;; end-handle: Last attribute in the service
  ;; value: Service UUID
  ;;
  ;; Issue new reads until ATT-ERROR = not found (#x0A)
  (let ((rsp t)
        (start-handle #x0001))
    (loop while rsp
          nconcing
          (progn
            ;; TODO: better story for errors
            (setf rsp (att-read-by-group-type
                       hci conn (encode-uuid +gatt-uuid-primary-service+)
                       start-handle))
            (when rsp
              (setf start-handle
                    (1+ (last-handle rsp)))
              ;; Do some translatin'
              (mapcar
               (lambda (el)
                 (list
                  :handle (getf el :handle)
                  :type :service
                  :end-handle   (getf el :end-handle)
                  ;; TODO: 128-bit
                  :uuid  (decode-c-int (getf el :value))))
               rsp))))))

(defun att-decode-read-by-type-rsp (data)
  (let ((el-len (- (pull-int data :u8) 2)))
    (loop while data
          collecting
          (list :handle (pull-int data :u16)
                :value (pull data el-len)))))

(defun att-read-by-type (hci conn-handle start end uuid)
  (unless (> end start)
    (error "Bad handle range [~A:~A]" start end))
  (att-send hci conn-handle
            (att-make-packet :read-by-type-req
                             (append
                              (make-c-int :u16 start)
                              (make-c-int :u16 end)
                              uuid)))
  (let* ((rsp (att-receive hci conn-handle :read-by-type-rsp))
         (data (getf rsp :data)))
    (when rsp
      (unless (att-error? (pull-int data :u8))
        (att-decode-read-by-type-rsp data)))))

(defun decode-char-discovery (attribute-list)
  (list :properties (nth 0 attribute-list)
        :value-handle (nth 1 attribute-list)
        :uuid (nth 2 attribute-list)))

(defun discover-chars (hci conn start end)
  ;; Discover all characteristics
  ;; [v5.4 p1494]
  ;;
  ;; response (opcode len (handle value))
  ;; where value:
  ;; - char props
  ;; - value handle
  ;; - uuid
  (let ((rsp t))
    (loop
      until (or (not rsp)
                (= end start))
      nconcing
      (progn
        ;; TODO: better error handling
        (setf rsp (att-read-by-type
                   hci conn start end
                   (encode-uuid +gatt-uuid-characteristic+)))
        (when rsp
          ;; Calculate handle for next request
          (setf start (1+ (getf (car (last rsp)) :handle)))
          (mapcar
           (lambda (el)
             (let* ((data (getf el :value))
                    (uuid-size (- (length data) 3)))
               (list
                :handle (getf el :handle)
                :type :characteristic-declaration
                :properties (pull-int data :u8)
                :value-handle (pull-int data :u16)
                :uuid (if (= uuid-size 2)
                          (pull-int data :u16)
                          (pull data uuid-size)))))
           rsp))))))

(defun char-end-handle (value-handle chars)
  (loop for char in chars
        do (when (> (getf char :value-handle) value-handle)
             (return-from char-end-handle (- (getf char :handle) 1))))
  ;; If we reach the end, let's just use value-handle + 1
  (1+ value-handle))

(defun discover-char-descriptors (hci conn char chars)
  ;; Discover all characteristic descriptors
  ;; [v5.4 p1496]
  ;;
  ;; response (opcode len (handle value))
  ;; where value:
  ;; - char props
  ;; - value handle
  ;; - uuid
  (let* ((rsp t)
         (value-handle (getf char :value-handle))
         (end (char-end-handle value-handle chars))
         (start (1+ value-handle)))
    (loop
      until (or (not rsp) (> start end))
      nconcing
      (progn
        ;; TODO: better error handling
        (setf rsp (att-find-information hci conn start end))
        (when rsp
          ;; Calculate handle for next request
          (setf start (1+ (getf (car (last rsp)) :handle)))
          (format t "DESC ~A~%" rsp)
          ;; TODO: add a dedicated CCCD type
          (mapcar
           (lambda (el)
             ;; Skip the service declarations, we already have them
             (unless (= (getf el :uuid-16) +gatt-uuid-primary-service+)
               (list
                :handle (getf el :handle)
                :type :characteristic-descriptor
                :uuid (getf el :uuid-16)
                :uuid128 (getf el :uuid-128))))
             rsp))))))

(defun gattc-discover (hci conn)
  (let* ((services (discover-services hci conn))
         (characteristics
           (apply #'nconc
                  (mapcar
                   (lambda (s) (discover-chars
                                hci conn
                                (getf s :handle)
                                (getf s :end-handle)))
                   services)))

         (characteristic-values
           (mapcar
            (lambda (c)
              (list
               :handle (getf c :value-handle)
               :type :characteristic-value
               :uuid (getf c :uuid)))
            characteristics))

         (descriptors
           (delete
            nil
            (apply #'nconc
                   (mapcar
                    (lambda (c) (discover-char-descriptors
                                 hci conn c characteristics))
                    characteristics)))))

    (sort (nconc
           services characteristics characteristic-values descriptors)
          (lambda (a b)
            (< (getf a :handle) (getf b :handle))))
    ))

(defun gattc-print (table)
  (with-output-to-string (os)
    (format os "~%")
    (loop for attribute in table do
      (case (getf attribute :type)
        (:service
         (format os "~4,'.,X SERVICE ~X~%"
                 (getf attribute :handle)
                 (getf attribute :uuid)))
        (:characteristic-declaration
         (format os "~4,'.,X   CHARACTERISTIC (PROP ~2,X) (UUID ~4,'0,X)~%"
                 (getf attribute :handle)
                 (getf attribute :properties)
                 (getf attribute :uuid)))
        (:characteristic-value
         (format os "~4,'.,X     VALUE~%"
                 (getf attribute :handle)))
        (:characteristic-descriptor
         (when (eql (getf attribute :uuid) +gatt-uuid-cccd+)
           (format os "~4,'.,X     CCCD~%"
                   (getf attribute :handle))))))))

;; Mixing gatts and gattc.. you oughta know better jon
(defun gatt-find-handle (table uuid &key
                                      (type :characteristic-value)
                                      (start 1)
                                      (end #xFFFF))
  (format t "GATT-FIND-HANDLE uuid ~X start ~X end ~X~%" uuid start end)
  "Find a needle in a haystack"
  (getf
   (find-if
    (lambda (a)
      (and
       (eql type (getf a :type))
       (if uuid
           (eql uuid
                (if (eql type :service)
                    ;; The service UUID is in the data itself
                    (decode-c-int (funcall (getf a :read) 0 0) :u16)
                    (getf a :uuid)))
           t)))
    table
    :start (- (min (length table) start) 1)
    :end (min (length table) end))
   :handle))

(defun gattc-find-handle (table uuid)
  "Find the characteristic value handle of UUID"
  (gatt-find-handle table uuid))

(defun att-read (hci conn handle)
  (format t "READING ~X~%" handle)
  (att-send hci conn
            (att-make-packet :read-req
                             (append
                              (make-c-int :u16 handle))))
  (let* ((rsp (att-receive hci conn :read-rsp))
         (data (getf rsp :data)))
    (when rsp
      (if (att-error? (pull-int data :u8))
          (format t "ATT-READ-REQ ERROR: ~X~%" data)
          data))))

(defun find-gap-name-handle (table)
  ;; Search for the GAP name value attribute
  (gattc-find-handle table +gatt-uuid-gap-device-name+))

(defun read-gap-name (hci conn table)
  (let ((handle (find-gap-name-handle table)))
    (when handle
      (att-read hci conn handle))))

(defun find-hr-handle (table)
  ;; Search for the HR value attribute
  (gattc-find-handle table +gatt-uuid-heart-rate-measurement+))

(defun read-hr (hci conn table)
  (let ((handle (find-hr-handle table)))
    (when handle
      (att-read hci conn handle))))

(defun att-write (hci conn handle value)
  ;; TODO: MTU checks, yada yada
  (format t "WRITING ~X~%" handle)
  (att-send hci conn
            (att-make-packet :write-req
                             (append
                              (make-c-int :u16 handle)
                              value)))
  (let* ((rsp (att-receive hci conn :write-rsp))
         (data (getf rsp :data)))
    (when rsp
      (if (att-error? (pull-int data :u8))
          (format t "ATT-WRITE-REQ ERROR: ~X~%" data)
          data))))

(defun gattc-find-cccd-handle (table uuid value-handle)
  "Find the characteristic value handle of UUID"
  (getf
   (find-if
    (lambda (a) (and
                 (> (getf a :handle) value-handle)
                 (eql :characteristic-descriptor (getf a :type))
                 (eql uuid (getf a :uuid))))
    table)
   :handle))

(defconstant +gatt-cccd-mask-notification+ #x0001)

(defun gattc-subscribe (hci conn table value-uuid)
  (let ((cccd-handle (gattc-find-cccd-handle
                      table
                      +gatt-uuid-cccd+
                      (gattc-find-handle table value-uuid))))
    (unless cccd-handle (break))
    (att-write hci conn cccd-handle
               (make-c-int :u16 +gatt-cccd-mask-notification+))))

(defun nfy? (conn-handle gatt-handle)
  (lambda (p)
    (and (eql (car p) :acl)
         (eql (getf (cadr p) :conn-handle)
              conn-handle)
         (eql (getf (cadr p) :channel)
              +l2cap-att-chan+)
         ;; Don't really need to "decode" since it's only one byte
         (eql (decode-c-int (getf (cadr p) :data) :u8)
              (att-make-opcode :handle-value-ntf t))
         ;; This is two bytes tho
         (eql (decode-c-int (subseq (getf (cadr p) :data) 1 3))
              gatt-handle))))

(defun wait-for-notification (hci conn gatt-handle)
  (let ((att-packet
          (getf (receive-if hci (nfy? conn gatt-handle)) :data)))
    (pull-int att-packet :u8)           ; opcode
    (pull-int att-packet :u16)          ; handle
    att-packet))

(defun wait-for-heartrate (hci conn gattc-table)
  (let* ((value-handle (gattc-find-handle
                        gattc-table
                        +gatt-uuid-heart-rate-measurement+))
         (data (wait-for-notification
                hci conn
                value-handle))
         (flags (pull-int data :u8))
         (heartrate (pull-int data (if (logbitp 0 flags) :u16 :u8))))
    heartrate))

(defun gatts-make-attribute (type uuid closures &optional name)
  "Make ATTributes"
  (append
   (list :type type
         :uuid uuid
         :read (getf closures :read)
         :write (getf closures :write))
   (when name
     (list :name name))))

(defun read-spy (conn handle)
  (format t "GATTS-READ: conn ~X handle ~X~%" conn handle))

(defun write-spy (conn handle data)
  (format t "GATTS-WRITE: conn ~X handle ~X data ~X~%" conn handle data))

(gatts-make-attribute
 :characteristic-value
 +gatt-uuid-heart-rate-measurement+
 (list :read #'read-spy :write #'write-spy)
 "My characteristic")
 ; => (:TYPE :CHARACTERISTIC-VALUE :UUID 10807 :READ #<FUNCTION READ-SPY> :WRITE
 ; #<FUNCTION WRITE-SPY> :NAME "My characteristic")

(defun gatts-make-char-value (uuid closures &optional name)
  (gatts-make-attribute
   :characteristic-value
   uuid
   closures
   name))

(gatts-make-char-value +gatt-uuid-heart-rate-measurement+
                       (list :read #'read-spy :write #'write-spy)
                       "My characteristic value")
 ; => (:TYPE :CHARACTERISTIC-VALUE :UUID 10807 :READ #<FUNCTION READ-SPY> :WRITE
 ; #<FUNCTION WRITE-SPY> :NAME "My characteristic value")

(defun gatts-make-char-decl (uuid properties &optional name)
  ;; properties should be pre-encoded
  (gatts-make-attribute
   :characteristic-declaration
   +gatt-uuid-characteristic+
   ;; props are read-only
   (list :read (lambda (c h)
                 (declare (ignore c))
                 (append
                  (make-c-int :u8 properties)
                  (make-c-int :u16 (1+ h))
                  (if (listp uuid)
                      uuid
                      (make-c-int :u16 uuid)))))
   name))

(defconstant +gatt-characteristic-properties+
  (list
   :broadcast
   :read
   :write-no-rsp
   :write
   :notify
   :indicate
   :auth-signed-writes
   :extended-props
   ))

(position :read +gatt-characteristic-properties+)
 ; => 1 (1 bit, #x1, #o1, #b1)
(position :notify +gatt-characteristic-properties+)
 ; => 4 (3 bits, #x4, #o4, #b100)

(defun make-props (props)
  (reduce #'logior
          (mapcar
           (lambda (prop)
             (ash 1
                  (position prop +gatt-characteristic-properties+)))
           props)))

(make-props '(:read :write :notify))
 ; => 26 (5 bits, #x1A, #o32, #b11010)

(gatts-make-char-decl
 +gatt-uuid-heart-rate-measurement+
 (make-props '(:read :notify))
 "Some useful note")
 ; => (:TYPE :CHARACTERISTIC-DECLARATION :UUID 10243 :READ
 ; #<FUNCTION (LAMBDA (H) :IN GATTS-MAKE-CHAR-DECL) {12060B05CB}> :WRITE NIL
 ; :NAME "Some useful note")

(defun gatts-make-cccd (closures)
  (gatts-make-attribute
   :characteristic-descriptor
   +gatt-uuid-cccd+
   closures
   "CCCD"))

(gatts-make-cccd (list :read #'read-spy :write #'write-spy))
 ; => (:TYPE :CHARACTERISTIC-DESCRIPTOR :UUID 10498 :READ #<FUNCTION READ-SPY> :WRITE
 ; #<FUNCTION WRITE-SPY> :NAME "CCCD")

(defun gatts-make-service (uuid)
  ;; [v5.4 p1474] services should be grouped by UUID size
  ;; re-order chars by UUID size
  (gatts-make-attribute
   :service
   +gatt-uuid-primary-service+
   (list :read (lambda (c h) (declare (ignore c h))
                 (if (listp uuid)
                     uuid
                     (make-c-int :u16 uuid))))))

(gatts-make-service +gatt-uuid-heart-rate-service+)
 ; => (:TYPE :SERVICE :UUID 10240 :READ
 ; #<FUNCTION (LAMBDA (H) :IN GATTS-MAKE-SERVICE) {1202E1847B}> :WRITE NIL)

(defun read-value-uuid (attribute)
  (when (eql (getf attribute :type) :characteristic-declaration)
    (decode-c-int
     (subseq (funcall (getf attribute :read) 0 0) 3) :u16)))

(read-value-uuid
 (gatts-make-char-decl
  +gatt-uuid-heart-rate-measurement+
  (make-props '(:read :notify))
  "Some useful note"))
 ; => 10807 (14 bits, #x2A37)

(defun read-props (attribute)
  (when (eql (getf attribute :type) :characteristic-declaration)
    (list :properties
          (decode-c-int (funcall (getf attribute :read) 0 0) :u8))))

(read-props
 (gatts-make-char-decl
  +gatt-uuid-heart-rate-measurement+
  (make-props '(:read :notify))
  "Some useful note"))
 ; => (:PROPERTIES 18)

(defun gatts-make-table (&rest attributes)
  ;; Let's go with simple for now
  ;; TODO: check uuid16 and uuid128 ordering
  (let ((handle 0))
    (loop for att in attributes
          collecting
          (append (list :handle (incf handle))
                  (read-props att)
                  att))))

;; ;; TODO: make a convenience macro that looks like this
;; (make-gatt
;;  (+gatt-uuid-heart-rate-service+
;;   (+gatt-uuid-heart-rate-measurement+
;;    (list (:read #'read-spy))
;;    '(:read :notify)))
;;  )

(defparameter *active-conns* '())

(defun get-address (conn)
  (getf *active-conns* conn))

;; cccd storage is just a plist (address . value)
(defun make-cccd-storage ()
  (let ((cccd-db))
    (list

     :read
     (lambda (conn handle)
       (declare (ignore handle))
       (getf cccd-db (get-address conn)))

     :write
     (lambda (conn handle value)
       (declare (ignore handle))
       (setf (getf cccd-db (get-address conn)) value)))))

(defparameter *gatts-table*
  (gatts-make-table
   (gatts-make-service +gatt-uuid-heart-rate-service+)
   (gatts-make-char-decl +gatt-uuid-heart-rate-measurement+ (make-props '(:read :notify)))
   (gatts-make-char-value +gatt-uuid-heart-rate-measurement+ (list :read #'read-spy))
   (gatts-make-cccd (make-cccd-storage))
   ))

(defun read-cccd (conn table value-handle)
  (let ((cccd-handle
          (gattc-find-cccd-handle table +gatt-uuid-cccd+ value-handle)))
    (when cccd-handle
      (funcall
       (getf (nth (- cccd-handle 1) table) :read)
       conn
       cccd-handle))))

(gattc-print *gatts-table*)
;  => "
; ...1 SERVICE 2800
; ...2   CHARACTERISTIC (PROP 12) (UUID 2803)
; ...3     VALUE
; ...4     CCCD
; "

(defun att-error-rsp (opcode handle code)
  (att-make-packet :error-rsp
                   (append
                    (make-c-int :u8 (att-make-opcode opcode t))
                    (make-c-int :u16 handle)
                    (make-c-int :u8 (att-make-error code)))))

(defun att-make-error (code)
  (getf +att-errors+ code))

(defun gatt-find-service (table uuid start end)
  ;; Return one service at a time
  (let* ((service-start
           (gatt-find-handle
            table uuid :type :service :start start :end end))
         ;; Service ends where next service begins
         (service-end
           (when service-start
             (gatt-find-handle
              table nil :type :service :start (1+ service-start)))))
    (when service-start
      (list :start service-start
            :end (if service-end (- service-end 1) #xFFFF)))))

(gatt-find-service
 *gatts-table* +gatt-uuid-heart-rate-service+ 1 #xFFFF)
 ; => (:START 1 :END 65535)

(defun gatts-find-service-rsp (table uuid search-start search-end)
  (destructuring-bind (&key start end)
      (gatt-find-service table uuid search-start search-end)
    (if start
        (att-make-packet :find-by-type-value-rsp
                         (append
                          (make-c-int :u16 start)
                          (make-c-int :u16 end)))
        (att-error-rsp :find-by-type-value-req
                       0 :attribute-not-found))))

(defun gatts-process-find-by-type-value (conn req)
  (declare (ignore conn))
  (let* ((start (pull-int req :u16))
         (end (pull-int req :u16))
         (type (pull-int req :u16))
         ;; TODO: 128b
         (uuid (pull-int req :u16)))
    (if (eql type +gatt-uuid-primary-service+)
        (gatts-find-service-rsp *gatts-table* uuid start end)
        (att-error-rsp
         :find-by-type-value-req 0 :request-not-supported))))

(defun gatts-find-char-rsp (table search-start search-end)
  (let* ((handle
           (gatt-find-handle table +gatt-uuid-characteristic+
                             ;; type is a bit redundant here..
                             :type :characteristic-declaration
                             :start search-start :end search-end))
         (read-fn (when handle
                    (getf (nth (- handle 1) table) :read))))
    (if handle
        (att-make-packet :read-by-type-rsp
                         (append
                          ;; handle 2 service decl data 1+2+2
                          (make-c-int :u8 (+ 2 5))
                          (make-c-int :u16 handle)
                          (funcall read-fn 0 handle)))
        (att-error-rsp :read-by-type-req
                       search-start :attribute-not-found))))


(defun gatts-process-read-by-type (conn req)
  (declare (ignore conn))
  (let* ((start (pull-int req :u16))
         (end (pull-int req :u16))
         (type (pull-int req :u16)))
    (format t "READ-BY-TYPE-REQ start ~X end ~X~%" start end)
    (if (eql type +gatt-uuid-characteristic+)
        (gatts-find-char-rsp *gatts-table* start end)
        (att-error-rsp
         :read-by-type-req 0 :request-not-supported))))

(defun gatts-find-info-rsp (table start end)
  (let ((information-data
          (apply #'nconc
                 (mapcar
                  (lambda (a)
                    (append (make-c-int :u16 (getf a :handle))
                            (make-c-int :u16 (getf a :uuid))))
                  (subseq table
                          (- (min (length table) start) 1)
                          (min (length table) end))))))
    (if information-data
        (att-make-packet :find-information-rsp
                         (append
                          (make-c-int :u8 #x01)
                          information-data))
        (att-error-rsp :find-information-req
                       start :attribute-not-found))))

(gatts-find-info-rsp *gatts-table* 4 #xFFFF)
 ; => (5 1 4 0 2 41)
(gatts-find-info-rsp *gatts-table* 1 #xFFFF)
 ; => (5 1 1 0 0 40 2 0 3 40 3 0 55 42)
(gatts-find-info-rsp *gatts-table* 6 #xFFFF)
 ; => (1 4 6 0 10)

(defun gatts-process-find-information (conn req)
  (declare (ignore conn))
  (let* ((start (pull-int req :u16))
         (end (pull-int req :u16)))
    (format t "FIND-INFO-REQ start ~X end ~X~%" start end)
    (gatts-find-info-rsp *gatts-table* start end)))

(defun gatts-process-write (conn req)
  (let* ((handle (pull-int req :u16)))
    (format t "CCCD-WRITE-REQ handle ~X~%" handle)
    (funcall (getf (nth (- handle 1) *gatts-table*) :write) conn handle req)
    (att-make-packet :write-rsp '())))

;; Handling ATT server:
;; - wait for ACL packet
;;   - this can be swapped for a global "event"
;;   - TX requests can be "event"
;;   - even GUI updates
;; - if ATT, dispatch ATT rsp

(defun wait-for-att-request (hci conn)
  ;; Wait for the next ATT request
  (let* ((req (getf (receive-if hci (att? conn nil t)) :data))
         (op (pull-int req :u8))
         (op-name (plist-key +att-opcodes+ op)))
    (att-send
     hci conn
     (case op-name
      (:find-by-type-value-req
       (gatts-process-find-by-type-value conn req))
      (:read-by-type-req
       (gatts-process-read-by-type conn req))
      (:find-information-req
       (gatts-process-find-information conn req))
      (:write-req
       (gatts-process-write conn req))
      (t
       (att-error-rsp op-name 0 :request-not-supported))))))

(defun subbb? (conn table handle)
  (let ((cccd
          (read-cccd conn table handle)))
    (when cccd
      (= #x0001 (decode-c-int cccd)))))

(defun notify (hci conn handle value)
  (att-send hci conn
            (att-make-packet
             :handle-value-ntf
             (append
              (make-c-int :u16 handle)
              value))))

(defun encode-hr (bpm)
  (append
   (make-c-int :u8 #b110)
   (make-c-int :u8 bpm)))

(defun process-rx (hci)
  ;; Just print the packets for now
  (loop
    (let ((packet (receive-rxq hci)))
      (unless packet
        (return-from process-rx nil))
      (format t "RXQ: ~A~%" packet))))

(time
 (with-hci hci *h2c-path* *c2h-path*
   (format t "================ enter ===============~%")
   (hci-reset hci)
   (hci-read-buffer-size hci)
   (hci-allow-all-the-events hci)
   (hci-set-random-address hci #xC1234567890A)

   (hci-set-scan-param hci)
   (hci-set-scan-enable hci t)

   (format t "RX ~A~%" (receive hci))

   (let ((address (wait-for-scan-report hci (lambda (x) (declare (ignore x)) t)))
         (conn-handle)
         (gattc-table)
         ;; Shadow active-conns
         (*active-conns* '())
         (gatts-hr-handle
           (gattc-find-handle *gatts-table* +gatt-uuid-heart-rate-measurement+)))

     ;; Stop scanning
     (hci-set-scan-enable hci nil)

     ;; Initiate the connection
     (format t "Connecting to peer ~A~%" address)
     ;; Some weird mangling happens if I don't copy, hmm..
     (hci-create-connection hci (copy-tree address))

     ;; Wait for the connection event
     (setf conn-handle (wait-for-conn hci))
     (setf (getf *active-conns* conn-handle)
           (decode-c-int (getf address :address) :u32))

     ;; Pop channel selection evt
     (format t "RX ~A~%" (receive hci))

     ;; Upgrade MTU
     (format t "Upgrading MTU..~%")
     (format t "Negotiated MTU ~A~%" (att-set-mtu hci conn-handle 255))
     ;; Wait for NCP belonging to ATT REQ
     (format t "NCP: ~A~%" (wait-for-ncp hci conn-handle))

     ;; Discover GATT
     (setf gattc-table (gattc-discover hci conn-handle))
     (format t "Discovered: ~%~A~%" (gattc-print gattc-table))
     (setf *test* gattc-table)

     ;; Read the device name
     (format t "Read GAP Device Name: ~A~%"
             (from-c-string
              (read-gap-name hci conn-handle gattc-table)))

     (format t "Active conns: ~X~%" *active-conns*)
     (format t "CCCD before: ~X~%"
             (read-cccd conn-handle *gatts-table* gatts-hr-handle))

     ;; 4-step discovery
     (loop for i from 0 to 3 do
       (wait-for-att-request hci conn-handle))

     (format t "CCCD after: ~X~%"
             (read-cccd conn-handle *gatts-table* 3))

     (when (subbb? conn-handle *gatts-table* gatts-hr-handle)
       (notify hci conn-handle gatts-hr-handle (encode-hr 125))
       (notify hci conn-handle gatts-hr-handle (encode-hr 95)))

     ;; Peer logs are more readable this way
     (sleep .5)

     ;; Subscribe to HR
     (gattc-subscribe
      hci
      conn-handle
      gattc-table
      +gatt-uuid-heart-rate-measurement+)

     ;; Wait for some HR notifications
     (loop for i from 0 to 5 do
     (format t "Heart rate: ~A BPM~%"
             (wait-for-heartrate hci conn-handle gattc-table)))

     ;; Disconnect
     (format t "Disconnecting from conn-handle ~A~%" conn-handle)
     (hci-disconnect hci conn-handle)
     (wait-for-disconn hci)

     ;; Process ignored packets
     (process-rx hci)
     )

   ;; (format t "HCI: ~X~%" hci)
   (format t "================ exit ===============~%")
   ))
