;; deps
(require 'asdf)
(require :local-time)
(require :bordeaux-threads)
(require :sb-concurrency)
(require :sb-posix)
(require :cserial-port)
(require :ironclad)

(defpackage :host
  (:use :common-lisp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :host))

;; get'em debug frames
(declaim (optimize (debug 3)))

;;;;;;;;;;;;; logging

(defparameter *log-levels*
  '(:tra (5 "TRAC")
    :dbg (4 "DEBG")
    :inf (3 "INFO")
    :wrn (2 "WARN")
    :err (1 "ERRR")
    ))

(defparameter *current-log-level* :inf)
;; (defparameter *current-log-level* :dbg)
(defparameter *log-time* t)
(defparameter *log-lock* (bt:make-lock "logs"))
(defparameter *log-line-sink*
  (lambda (line)
    (write-string line *error-output*)))

(defun yeet-log-message (severity)
  (let ((current-severity (nth 0 (getf *log-levels* *current-log-level*)))
        (msg-severity (nth 0 (getf *log-levels* severity))))
    (> msg-severity current-severity)))

(defun poor-mans-log (severity message args)
  (unless (yeet-log-message severity)
    (bt:with-lock-held (*log-lock*)
      ;; let's talk about optimization later ok?
      (let ((log-line (apply #'format (append
                                       '(nil)
                                       (list message)
                                       args))))
        (funcall *log-line-sink*
                 (format nil "[~A][~A] ~A: ~A~%"
                         (if *log-time* (local-time:now) "")
                         (bt:thread-name (bt:current-thread))
                         (nth 1 (getf *log-levels* severity))
                         log-line))))))

(defun log-trace (message &rest args) (poor-mans-log :tra message args))
(defun log-dbg (message &rest args) (poor-mans-log :dbg message args))
(defun log-inf (message &rest args) (poor-mans-log :inf message args))
(defun log-wrn (message &rest args) (poor-mans-log :wrn message args))
(defun log-err (message &rest args) (poor-mans-log :err message args))

;;;;;;;;;;;;; general utils

(defun px (l)
  (format nil "~{~2,'0X~^ ~}" l))

(defun pxx (l)
  (format nil "0x~{~2,'0X~}" l))

(defparameter +u64-max+ (ldb (byte 64 0) -1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-range (max)
    (loop for number from 0 to (- max 1) collect number))

  (defun make-be-range (max)
    (loop for number from (- max 1) downto 0 collect number))

  (defun extract-byte (number index)
    (ldb (byte 8 (* index 8)) number))

  (defun make-uint (octets number &optional big-endian)
    (loop for pos in (if big-endian
                         (make-be-range octets)
                         (make-range octets))
          collect (extract-byte number pos)))

  ;; (make-uint 4 8000)
  ; => (64 31 0 0)

  ;; (make-uint 4 8000 t)
  ; => (0 0 31 64)

  (defun u2b (type)
    (case type
      (:u8 1)
      (:u16 2)
      (:u32 4)
      (:u64 8)
      (:u128 16)))

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

  ;; (type->octets :u32)
  ; => 4 (3 bits, #x4, #o4, #b100)
  ;; (type->octets :bt-addr)
  ; => 6 (3 bits, #x6, #o6, #b110)

  (defun make-c-int (type value &optional big-endian)
    (make-uint (type->octets type) value big-endian)))

(make-c-int :u8 #xFF)
 ; => (255)
(make-c-int :u32 8000)
 ; => (64 31 0 0)
(make-c-int :u32 8000 t)
 ; => (0 0 31 64)

(defun decode-c-int (bytes &optional type)
  (let ((result 0)
        (data (if type
                  (subseq bytes 0 (min (length bytes) (u2b type)))
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

(defun read-bytes (bytes stream)
  (let ((b
          (if (equal bytes 1)
              (list (when (open-stream-p stream)
                      (read-byte stream)))
              (loop for byte from 0 to (- bytes 1)
                    collect
                    (when (open-stream-p stream)
                      (read-byte stream))))))
    ;; (format nil "read-bytes: ~A" b)
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

;;;;;;;;;;;;; HCI packet-building

(defparameter +h4-types+
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

    :le-enable-encryption
    (#x2019 (:handle :u16
             :rand :u64
             :ediv :u16
             :ltk (list :u8))
     nil)

    :le-ltk-request-reply
    (#x201a (:handle :u16
             :ltk (list :u8))
     (:status :u8
      :conn-handle :u16))

    :le-ltk-request-neg-reply
    (#x201b (:handle :u16)
     (:status :u8
      :conn-handle :u16))

    :le-remote-conn-param-req-neg-reply
    (#x2021 (:handle :u16
             :reason :u8)
     (:status :u8
      :conn-handle :u16))

    :le-conn-update
    (#x2013 (:handle :u16
             :interval-min :u16
             :interval-max :u16
             :max-latency :u16
             :timeout :u16
             :min-ce-len :u16
             :max-ce-len :u16))

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

(defun decode-adv-addr-type (type)
  (case (logand type #b11)
    (#b00 :public)
    (#b01 :random)
    (#b10 :public-rpa)
    (#b11 :random-rpa)))

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

(defun decode-le-ltk-request (payload)
  (list
   :le-ltk-request
   (list
    :conn-handle (pull-int payload :u16)
    :random (pull payload 8)
    :ediv (pull-int payload :u16))))

(defun decode-le-remote-conn-param-req (payload)
  (list
   :le-remote-conn-param-req
   (list
    :conn-handle (pull-int payload :u16)
    :interval-min (pull-int payload :u16)
    :interval-max (pull-int payload :u16)
    :max-latency (pull-int payload :u16)
    :timeout (pull-int payload :u16))))

(defun decode-le-conn-update (payload)
  (list
   :le-conn-update
   (list
    :status (pull-int payload :u8)
    :conn-handle (pull-int payload :u16)
    :interval (pull-int payload :u16)
    :latency (pull-int payload :u16)
    :timeout (pull-int payload :u16))))

(defun decode-le-meta (payload)
  (let ((sub (pull-int payload :u8)))
    (case sub
      (#x01 (decode-conn-complete payload))
      (#x02 (decode-adv-report payload))
      (#x03 (decode-le-conn-update payload))
      (#x05 (decode-le-ltk-request payload))
      (#x06 (decode-le-remote-conn-param-req payload))
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

(defun evt-encryption-change (payload)
  (list :encryption-change
        (list :status (pull-int payload :u8)
              :handle (pull-int payload :u16)
              :enabled (pull-int payload :u8))))

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
    #x08 evt-encryption-change
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

    ;; (log-dbg (format nil "OP: ~x param-spec: ~A params ~A" opcode param-spec params))
    (append
     (make-c-int :u16 opcode)
     (make-c-int :u8 (length serialized-params))
     serialized-params)))

;; HCI cmds in plist
;; name: (plist of :param-name :type)
(format nil "~A" (make-hci-cmd :write-default-data-length
                                 :tx-octets 200
                                 :tx-time-us 1000))
; cmd WRITE-DEFAULT-DATA-LENGTH op 2024 param-spec: (TX-OCTETS U16 TX-TIME-US U16) params (TX-OCTETS
;                                                                                          C8
;                                                                                          TX-TIME-US
;                                                                                          3E8)
;  => "(36 32 4 200 0 232 3)
; "

(defparameter *hci-log* '())
(defparameter *hci-log-start-time* (get-internal-real-time))

(defun hci-log-reset ()
  (setf *hci-log* '())
  (setf *hci-log-start-time* (get-internal-real-time)))

(defun hci-log (direction packet)
  (push (list
         :timestamp (- (get-internal-real-time) *hci-log-start-time*)
         :direction direction
         :packet packet)
        *hci-log*))

(defun h4-parse-opcode (packet)
  "Looks up the H4 opcode"
  (plist-key +h4-types+ (car packet)))

(defun hci-header-len (opcode)
  "Returns the length of the HCI packet header field"
  (case opcode
    (:evt 2)
    (:acl 4)
    (:iso 4)
    (t (error "unknown h4 packet type"))))

(defun hci-header-len-field (opcode)
  "Returns the offset and the size of the length field"
  (case opcode
    (:evt '(1 1))
    (:acl '(2 2))
    (:iso '(2 2))
    (t (error "unknown h4 packet type"))))

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
    ;; (log-dbg (format nil "read op"))
    (setf packet (read-bytes 1 stream))
    ;; (log-dbg (format nil "packet: ~A" packet))

    ;; parse h4 opcode
    (setf opcode (h4-parse-opcode packet))

    ;; read HCI packet header
    ;; (log-dbg (format nil "read header"))
    (setf header (read-bytes (hci-header-len opcode) stream))
    (setf packet (append packet header))
    ;; (log-dbg (format nil "header: ~X" header))
    ;; (log-dbg (format nil "packet: ~A" packet))

    ;; parse hci-packet-length from header & read payload
    ;; (log-dbg (format nil "read payload"))
    (setf payload (read-bytes
                   (hci-parse-len opcode packet)
                   stream))
    (setf packet (append packet payload))
    (log-trace (format nil "RX[H4]: ~X" packet))

    ;; return raw packet and parsed packet
    (list
     :opcode opcode
     :header header
     :payload payload
     :raw packet)))

(defun append-to-acl-in (packetizer fragment)
  (setf (getf packetizer :acl-in)
        (append (getf packetizer :acl-in) fragment)))

(defun decode-l2cap (conn fragment)
  (list :conn-handle conn
        :length (pull-int fragment :u16)
        :channel (pull-int fragment :u16)
        :data fragment))

(defparameter +l2-hdr-size+ 4)

(defun complete-acl (packetizer conn fragment)
  ;; for now only one conn supported
  (let* ((l2pac (append-to-acl-in packetizer fragment))
         (l2size
           (+ +l2-hdr-size+
              (decode-c-int (subseq l2pac 0 2))))
         (current-size (length l2pac)))
    ;; TODO: error handling?
    (log-trace (format nil "[ACL-APPEND] (~A/~A) ~X" current-size l2size l2pac))
    (if (= l2size current-size)
        (progn
          (setf (getf packetizer :acl-in) '())
          (decode-l2cap conn l2pac))
        nil)))

(defun decode-acl-type (header)
  (ldb (byte 2 12) (logand (decode-c-int header) #xB000)))

(defun decode-hci-acl (packetizer header payload)
  (let ((conn-handle (logand (decode-c-int header) #x0FFF))
        (pb-flag (decode-acl-type header)))
    (if (zerop pb-flag)
        ;; This branch is dead code with the current controller rn
        (decode-l2cap conn-handle payload)
        (complete-acl packetizer conn-handle payload))))

(defun receive (packetizer)
  "Receive and decode a single HCI packet"
  ;; initial implementation is H4
  (let ((stream (getf packetizer :c2h)))
    (let* ((packet (rx-h4 stream))
           (opcode (getf packet :opcode))
           (header (getf packet :header))
           (payload (getf packet :payload)))
      (hci-log :c2h (getf packet :raw))
      (case opcode
        (:evt (list :evt (decode-hci-event header payload)))
        (:acl (list :acl (decode-hci-acl packetizer header payload)))
        (t (error "doesn't look like anything to me"))))))

(defun add-to-rxq (hci packet)
  (when (cadr packet)
    (push packet (getf hci :rxq))))

(defun receive-rxq (hci &optional predicate)
  (if (not predicate)
      (let ((packet (car (last (getf hci :rxq)))))
        (when packet
          (setf (getf hci :rxq)
                (butlast (getf hci :rxq)))
          packet))
      (let ((packet (find-if predicate (getf hci :rxq))))
        (when packet
          (setf (getf hci :rxq)
                (delete-if predicate (getf hci :rxq)))
          packet))))

(defun make-signal ()
  (sb-concurrency:make-gate))

(defun wait-for-signal (var)
  (sb-concurrency:wait-on-gate var))

(defun send-signal (var)
  (sb-concurrency:open-gate var))

(defun make-mailbox (name)
  (sb-concurrency:make-mailbox :name name))

(defun drain-mailbox (mb)
  (sb-concurrency:receive-message mb :timeout 0.1))

(defun send-to-hci (hci packet)
  (sb-concurrency:send-message (getf hci :tx-mailbox) packet))

(defun send (hci type payload)
  "Format a payload into H4 and send to hci device"
  (let ((packet (make-h4 type payload)))
    (log-trace (format nil "TX: ~x" packet))
    (hci-log :h2c packet)
    (send-to-hci hci packet)))

(defun receive-in-thread (packetizer)
  (bt:make-thread
   (lambda ()
     (loop
       (ignore-errors
        (sb-concurrency:send-message
         (getf packetizer :rx-mailbox)
         (receive packetizer)))))
   :name "Packetizer RX thread"))

(defun write-dumdum (data stream)
  (loop for byte in data
          do (write-byte byte stream)))

(defun send-in-thread (packetizer)
  (bt:make-thread
   (lambda ()
     (loop
       (ignore-errors
        (let ((packet
                (sb-concurrency:receive-message
                 (getf packetizer :tx-mailbox))))
          (when packet
            (log-trace "HWTX: ~X" packet)
            (write-dumdum packet (getf packetizer :h2c))
            (log-trace "HWTX OK")
            )))))
   :name "Packetizer TX thread"))

(defun wait-next-hci-rx (hci)
  (add-to-rxq
   hci (sb-concurrency:receive-message (getf hci :rx-mailbox))))

(defun drain-rxq (hci)
  "Move items into our owned HCI RX queue"
  (loop until (sb-concurrency:mailbox-empty-p (getf hci :rx-mailbox)) do
    (wait-next-hci-rx hci)))

(declaim (ftype function process-hci))

(defun do-idle-work (hci)
  (log-trace (format nil "IDLE"))
  (loop
    (let ((packet (receive-rxq hci)))
      (log-trace (format nil "IDLE-LOOP packet ~X" packet))
      (if packet
          (process-hci hci packet)
          (return-from do-idle-work nil)))))

(defun receive-if (hci predicate)
  (progn
    (drain-rxq hci)
    (loop
      (let ((packet (receive-rxq hci predicate)))
        (when packet
          ;; strip the H4 header
          (return-from receive-if (cadr packet)))
        (unless (do-idle-work hci)
          ;; Sleep until next packet if queue is empty
          (wait-next-hci-rx hci)
          )))))

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

;;;;;;;;;;;;; btsnoop

(defun write-bytes (bytelist stream)
  (loop for byte in bytelist do
    (write-byte byte stream)))

(defun setbit (shift predicate)
  (ash (if predicate 1 0) shift))

(defun make-flags (packet)
  (logior
   ;; 1: c2h  0: h2c
   (setbit 0 (eql :c2h (getf packet :direction)))
   ;; 1: data 0: cmd/evt
   (setbit 1 (not (eql (getf +h4-types+ :acl) (car (getf packet :packet)))))))

(defun make-header (packet)
  (let* ((original-length (length (getf packet :packet)))
         (included-length original-length)
         (flags (make-flags packet))
         ;; like that's not a lie lol
         (cumulative-drops 0)
         ;; don't really care about correct timestamp
         (timestamp (getf packet :timestamp)))
    (append
     (make-c-int :u32 original-length t)
     (make-c-int :u32 included-length t)
     (make-c-int :u32 flags t)
     (make-c-int :u32 cumulative-drops t)
     (make-c-int :u64 timestamp t))))

(defun write-btsnoop (path hci-log)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    ;; Magic
    (write-bytes (to-c-string "btsnoop" t) stream)
    ;; Version
    (write-bytes (make-c-int :u32 1 t) stream)
    ;; Datalink type: UART
    (write-bytes (make-c-int :u32 1002 t) stream)

    ;; Packets
    (loop for packet in (reverse hci-log) do
      (write-bytes (make-header packet) stream)
      (write-bytes (getf packet :packet) stream))))

(defun hci-log-write ()
  (write-btsnoop "./snoop.log" *hci-log*))

;;;;;;;;;;;;; host

(defun make-hci-packetizer (h2c-stream c2h-stream)
  (list
   :h2c h2c-stream
   :c2h c2h-stream
   :rx-mailbox '()
   :tx-mailbox '()
   :acl-in '()
   :acl-tx-size 0
   :acl-tx-num 0
   :acl-rx-size 0))

(defun make-controller ()
  (list
   :rx-mailbox (make-mailbox "HCI H <= C")
   :tx-mailbox (make-mailbox "HCI H => C")
   :stop-signal (make-signal)
   :rxq '()
   :random-address 0))

;;;;;;;;;;;;; script

;; To use on a real device
;; socat -x /dev/ttyACM0,rawer,b115200 'GOPEN:/tmp/lhost/uart.h2c!!GOPEN:/tmp/lhost/uart.c2h'
;;
;; Build using devcontainer: https://github.com/narvalotech/zephyr/tree/devcontainer
;; cd samples/bluetooth/hci_uart
;; west build -b nrf52840dongle/nrf52840 -- -DEXTRA_CONF_FILE='overlay-all-bt_ll_sw_split.conf'
;;
;; Use nrfdfu to flash:
;; nix-shell -p cargo
;; cargo install nrfdfu
;; ~/.cargo/bin/nrfdfu samples/bluetooth/hci_uart/build/zephyr/zephyr.elf

(defparameter sizes '(:acl-tx-size 0
                      :acl-rx-size 1))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro with-fifo-packetizer (instance h2c-path c2h-path &body body)
  (with-gensyms (h2c c2h)
    `(with-open-stream (,h2c (open-simplex-fd ,h2c-path t))
       (with-open-stream (,c2h (open-simplex-fd ,c2h-path nil))
         (let ((,instance (make-hci-packetizer ,h2c ,c2h)))
           (progn ,@body)
         )))))

(defmacro with-serial-packetizer (instance serial-port-path &body body)
  `(cserial-port:with-serial
       (rs ,serial-port-path :baud-rate 1000000 :data-bits 8 :stop-bits 1 :parity :none)
     (let* ((serial-stream (cserial-port:make-serial-stream rs))
            (,instance (make-hci-packetizer serial-stream serial-stream)))
       (progn ,@body)
       )))

(defun hci-send-cmd (hci cmd)
  "Send a command and check it's return status. Return status/params if no error."

  (log-dbg (format nil "CMD-TX: ~x" cmd))
  (send hci :cmd cmd)

  (let ((response (receive-cmd hci)))
    ;; Here `response` is an HCI event object, e.g.
    ;; (CMD-COMPLETE (NCMD 1 OPCODE C03 PARAMS (STATUS 0)))
    (log-dbg (format nil "CMD-RX: ~x" response))

    (if (eql (car response) :cmd-status)
        (let* ((status (getf (nth 1 response) :status)))
          (unless (zerop status)
            (log-err (format nil "cmd failed: status 0x~x" status)))
          status)

        ;; cmd-complete
        (let* ((data (nth 1 response))
               (params (getf data :params))
               (status (getf params :status)))

          (if (not (equal status 0))
              (progn
                (log-err (format nil "cmd failed: status 0x~x" status))))

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
                 :interval-min 6
                 :interval-max 10
                 :max-latency 0
                 :supervision-timeout 200
                 :min-connection-event-length 0
                 :max-connection-event-length #xffff)))

;; TODO: define all error codes
(defparameter +remote-user-terminated+ #x13)

(defun hci-disconnect (hci handle)
  (hci-send-cmd
   hci
   (make-hci-cmd :disconnect
                 :handle handle
                 :reason +remote-user-terminated+)))

(defparameter +ad-types+
  (list :flags #x01
        :class-uuid-16-incomplete #x02
        :class-uuid-16-complete #x03
        :class-uuid-32-incomplete #x04
        :class-uuid-32-complete #x05
        :class-uuid-128-incomplete #x06
        :class-uuid-128-complete #x07

        :solicitation-uuid-16 #x14
        :solicitation-uuid-32 #x1f
        :solicitation-uuid-128 #x15

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

(defun make-ad-name (name)
  (make-ad :name-complete (to-c-string name)))

(make-ad-name "hello")
 ; => (6 9 104 101 108 108 111)
(make-ad-name "🎉")
 ; => (5 9 240 159 142 137)

(defparameter *test-ad*
  (append
   (make-ad :flags '(#x01)) ; LE General discoverable
   (make-ad-name "Bluey McBleface")))

(defun rmx (uuidstr)
  (let ((idx (search "xxxx" uuidstr :test #'equalp)))
    (if idx
        (replace uuidstr "0000" :start1 idx :end1 (+ idx 4))
        uuidstr)))

(defun parse-uuid-128 (uuidstr)
  (let ((uuidstr (remove #\- (rmx uuidstr))))
    (loop for i from 0 below (length uuidstr) by 2
          for two-chars = (subseq uuidstr i (min (+ i 2) (length uuidstr)))
          when two-chars
            collect (parse-integer two-chars :radix 16))))

(defun parse-uuid (uuidstr)
  (if (= 2 (length uuidstr))
      (make-c-int :u16 (parse-integer uuidstr :radix 16))
      (reverse (parse-uuid-128 uuidstr))))

(defun eq-any (a &rest values)
  (loop for b in values do
        (when (eql a b) (return t))))

(defun pretty-print-uuid (long-uuid)
  (let ((output ""))
    (loop for char across (format nil "~X" long-uuid) do
      (progn
        (setf output
              (concatenate 'string output (format nil "~A" char))))
      (when (eq-any (length output) 8 13 18 23)
        (setf output
              (concatenate 'string output "-"))))
    output))

(pretty-print-uuid #x6E400001B5A3F393E0A9E50E24DCCA9E)
 ; => "6E400001-B5A3-F393-E0A9-E50E24DCCA9E"

(pretty-print-uuid #x6E40)
 ; => "6E40"

(defparameter +nus-uuid+ "6E400001-B5A3-F393-E0A9-E50E24DCCA9E")
(defparameter +battery-uuid+ "180F")
(px (parse-uuid +nus-uuid+))
 ; => "9E CA DC 24 0E E5 A9 E0 93 F3 A3 B5 01 00 40 6E"
(px (parse-uuid +battery-uuid+))
 ; => "0F 18"

(defun make-ad-solicited-uuid (uuid-strings &key (type :u16))
  "List of UUID strings to be solicited. 16b and 128b are supported."
  (let ((uuids (mapcar #'parse-uuid uuid-strings)))
    (make-ad (if (eql type :u16)
                 :solicitation-uuid-16
                 :solicitation-uuid-128) (apply #'nconc uuids))))

(px (make-ad-solicited-uuid (list +nus-uuid+) :type :u128))
 ; => "11 15 9E CA DC 24 0E E5 A9 E0 93 F3 A3 B5 01 00 40 6E"

(px (make-ad-solicited-uuid (list +battery-uuid+) :type :u16))
 ; => "03 14 0F 18"

(defun make-ad-mfg (manufacturer-id payload)
  (make-ad :manufacturer-specific
           (append (make-c-int :u16 manufacturer-id) payload)))

(defun parse-ad (input)
  (let ((encoded (copy-tree input))
        (len)
        (type))
    ;; LTV structure
    (loop while encoded
          nconc
          (progn
            (setf len (pull-int encoded :u8))
            (when (and (> len 0) (>= (length encoded) len))
              (log-trace "decoding len ~A enc ~A" len encoded)
              (setf type (pull-int encoded :u8))
              (decf len)
              (list
               (plist-key +ad-types+ type)
               (pull encoded len)))))))

(parse-ad *test-ad*)
 ; => (:FLAGS (1) :NAME-COMPLETE
 ; (66 108 117 101 121 32 77 99 66 108 101 102 97 99 101))

(search
 "mcble"
 (from-c-string
  (getf (parse-ad *test-ad*) :name-complete))
 :test #'equalp)
 ; => 6 (3 bits, #x6, #o6, #b110)

(defun evt? (evt-code)
  (lambda (packet)
    (eql (car (cadr packet)) evt-code)))

(defun wait-for-scan-report (hci predicate)
  ;; TODO: handle multiple reports
  (loop
    (let ((evt (receive-if hci (evt? :le-scan-report))))
      (let ((report (find-if predicate (getf (nth 1 evt) :reports))))
        (when report
          (return-from wait-for-scan-report
            (list :type (getf report :address-type)
                  :address (getf report :address))))))))

(defun decode-random-addr-type (raw-address)
  "Decode the address type from just the 6 MAC bytes"
  (case (ldb (byte 2 46) raw-address)
    (#x00 :nrpa)
    (#x01 :rpa)
    (#x10 :rfu)
    (#x11 :static)))

(defun make-address (address type)
  (list :address address
        :type type))

(defun wait-for-conn (hci)
  (let ((evt (receive-if hci (evt? :le-enh-conn-complete))))
    (list :handle
          (getf (nth 1 evt) :handle)
          :address
          (make-address
           (decode-c-int (getf (nth 1 evt) :peer-address) :u64)
           (getf (nth 1 evt) :peer-address-type)))))

(defun wait-for-disconn (hci)
  (let ((evt (receive-if hci (evt? :disconnection-complete))))
    (getf (nth 1 evt) :handle)))

(defun ncp? (conn-handle)
  (lambda (p)
    (and (eql (car p) :evt)
         (eql (car (cadr p)) :number-of-completed-packets)
         (eql (getf (cadr (cadr p)) :handle) conn-handle))))

(defun wait-for-ncp (hci handle)
  (receive-if hci (ncp? handle)))

;; Mandatory:
;; - error-rsp
;; - find-information-req
;; - read-req
(defparameter +att-errors+
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

(defparameter +att-opcodes+
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun att-make-opcode (op-name &optional single)
    (if (not single)
        (make-c-int :u8 (getf +att-opcodes+ op-name))
        (getf +att-opcodes+ op-name))))

(defparameter +att-requests+
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
  (unless conn-handle
    (error "conn handle is nil"))
  (when packet
    (hci-send-acl
     hci
     conn-handle
     (append
      (make-c-int :u16 (length packet))
      (make-c-int :u16 channel)
      packet))))

(defparameter +l2cap-att-chan+ #x0004)

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
  (when payload
    (log-dbg (format nil "ATT-TX ~X" payload))
    (l2cap-send hci conn-handle +l2cap-att-chan+ payload)))

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

(defun decode-handles-and-uuids (data &key 128-bit)
  (loop while data collecting
        (list :handle (pull-int data :u16)
              :uuid (decode-c-int
                     (pull data (if 128-bit 16 2))))))

(defun att-decode-find-information-rsp (data)
  (let ((128-bit (= 2 (pull-int data :u8))))
    (decode-handles-and-uuids data :128-bit 128-bit)))

(defun att-error? (opcode)
  (eql opcode (att-make-opcode :error-rsp t)))

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

(defparameter +gatt-uuid-primary-service+ #x2800)
(defparameter +gatt-uuid-characteristic+ #x2803)
(defparameter +gatt-uuid-cccd+ #x2902)
(defparameter +gatt-uuid-heart-rate-service+ #x180D)
(defparameter +gatt-uuid-heart-rate-measurement+ #x2A37)
(defparameter +gatt-uuid-gap-device-name+ #x2A00)
(defparameter +gatt-uuid-gap-ppcp+ #x2A04)

(defun encode-uuid (uuid)
  (if (<= uuid #xFFFF)
      (make-c-int :u16 uuid)
      (make-c-int :u128 uuid)))

(let* ((enc-uuid '(158 202 220 36 14 229 169 224 147 243 163 181 2 0 64 110)))
  (encode-uuid
   (decode-c-int
    (pull enc-uuid (length enc-uuid)))))
 ; => (158 202 220 36 14 229 169 224 147 243 163 181 2 0 64 110)

(encode-uuid +gatt-uuid-cccd+)
 ; => (2 41)

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
    (loop while (and rsp (< start-handle #xFFFF))
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
                :uuid (decode-c-int (pull data uuid-size)))))
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
          (log-dbg (format nil "DESC ~A" rsp))
          ;; TODO: add a dedicated CCCD type
          (mapcar
           (lambda (el)
             ;; Skip the service declarations, we already have them
             (unless (= (getf el :uuid) +gatt-uuid-primary-service+)
               (list
                :handle (getf el :handle)
                :type :characteristic-descriptor
                :uuid (getf el :uuid))))
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
         (format os "~4,'.,X     VALUE (UUID ~X)~%"
                 (getf attribute :handle)
                 (getf attribute :uuid)))
        (:characteristic-descriptor
         (when (eql (getf attribute :uuid) +gatt-uuid-cccd+)
           (format os "~4,'.,X     CCCD~%"
                   (getf attribute :handle))))))))

;; Mixing gatts and gattc.. you oughta know better jon
(defun gatt-find-handle (table uuid &key
                                      (type nil)
                                      (start 1)
                                      (end #xFFFF))
  (log-dbg (format nil "GATT-FIND-HANDLE uuid ~X start ~X end ~X ~A" uuid start end type))
  "Find a needle in a haystack"
  (getf
   (find-if
    (lambda (a)
      (and
       t
       (if type
           (eql type (getf a :type))
           t)
       (if uuid
           (eql uuid
                (if (eql type :service)
                    ;; The service UUID is in the data itself
                    (decode-c-int (funcall (getf a :read) 0 0)
                                  (if (> uuid #xFFFF) :u128 :u16))
                    (getf a :uuid)))
           t)))
    table
    :start (- (min (length table) start) 1)
    :end (min (length table) end))
   :handle))

(defun att-read (hci conn handle)
  (log-dbg (format nil "READING ~X" handle))
  (att-send hci conn
            (att-make-packet :read-req
                             (append
                              (make-c-int :u16 handle))))
  (let* ((rsp (att-receive hci conn :read-rsp))
         (data (getf rsp :data)))
    (when rsp
      (if (att-error? (pull-int data :u8))
          (progn
            (log-dbg (format nil "ATT-READ-REQ ERROR: ~X" data))
            nil)
          data))))

(defun find-gap-name-handle (table)
  ;; Search for the GAP name value attribute
  (gatt-find-handle table +gatt-uuid-gap-device-name+ :type :characteristic-value))

(defun read-gap-name (hci conn table)
  (let ((handle (find-gap-name-handle table)))
    (when handle
      (att-read hci conn handle))))

(defun find-hr-handle (table)
  ;; Search for the HR value attribute
  (gatt-find-handle table +gatt-uuid-heart-rate-measurement+ :type :characteristic-value))

(defun read-hr (hci conn table)
  (let ((handle (find-hr-handle table)))
    (when handle
      (att-read hci conn handle))))

(defun att-write (hci conn handle value &optional no-rsp)
  ;; TODO: MTU checks, yada yada
  (log-dbg (format nil "WRITING ~X" handle))
  (att-send hci conn
            (att-make-packet (if no-rsp :write-cmd :write-req)
                             (append
                              (make-c-int :u16 handle)
                              value)))
  (unless no-rsp
    (let* ((rsp (att-receive hci conn :write-rsp))
           (data (getf rsp :data)))
      (when rsp
        (log-dbg "WRITE RSP: ~X" rsp)
        (if (att-error? (pull-int data :u8))
            (log-dbg (format nil "ATT-WRITE-REQ ERROR: ~X" data))
            data)))))

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

(defparameter +gatt-cccd-mask-notification+ #x0001)

(defun gattc-subscribe (hci conn table value-uuid)
  (let ((cccd-handle (gattc-find-cccd-handle
                      table
                      +gatt-uuid-cccd+
                      (gatt-find-handle table value-uuid))))
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
  (let* ((value-handle (gatt-find-handle
                        gattc-table
                        +gatt-uuid-heart-rate-measurement+
                        :type :characteristic-value))
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
  (log-dbg (format nil "GATTS-READ: conn ~X handle ~X" conn handle))
  (make-c-int :u8 0))

(defun write-spy (conn handle data)
  (log-dbg (format nil "GATTS-WRITE: conn ~X handle ~X data ~X" conn handle data)))

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
                  (make-c-int
                   (if (> uuid #xFFFF) :u128 :u16) uuid))))
   name))

(defparameter +gatt-characteristic-properties+
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

(defun decode-props (encoded)
  (loop for pos from 0 to 7
        when (logbitp pos encoded)
        collect
             (nth pos +gatt-characteristic-properties+)))

(decode-props #b11010)
 ; => (:READ :WRITE :NOTIFY)

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
                 (make-c-int
                  (if (> uuid #xFFFF) :u128 :u16) uuid)))))

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
  (getf
   (getf
    (getf *active-conns* conn)
    :address)
   :address))                           ; yo dawg i herd you liked address

(defparameter *smp-context* (make-hash-table))

(defun get-smp-context (conn)
  (gethash conn *smp-context*))

(defun (setf get-smp-context) (new-value conn)
  (setf (gethash conn *smp-context*) new-value))

(defun encrypted? (conn)
  (getf (get-smp-context conn) :encrypted))

;; cccd storage is just a plist (address . value)
(defun make-cccd-storage ()
  (let ((cccd-db))
    (list

     :read
     (lambda (conn handle)
       (declare (ignore handle))
       (if (encrypted? conn)
        (getf cccd-db (get-address conn))
        :insufficient-authentication))

     :write
     (lambda (conn handle value)
       (declare (ignore handle))
       (if (encrypted? conn)
        (progn
          (setf (getf cccd-db (get-address conn)) value)
          nil)
        (progn
          (log-err (format nil "CCCD write error: ~A" :insufficient-authentication))
          :insufficient-authentication)))
     )))

(defparameter +gatt-uuid-gatt-service+ #x1801)
(defparameter +gatt-uuid-gatt-service-changed+ #x2A05)
(defparameter +gatt-uuid-gap-service+ #x1800)
(defparameter +gatt-uuid-gap-appearance+ #x2A01)

(defparameter *gatts-table*
  (gatts-make-table
   (gatts-make-service +gatt-uuid-gap-service+)
   (gatts-make-char-decl +gatt-uuid-gap-device-name+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-gap-device-name+ (list :read (lambda (c h)
                                                                    (declare (ignore c h))
                                                                    (to-c-string "LHost"))))
   (gatts-make-char-decl +gatt-uuid-gap-appearance+ (make-props '(:read)))
   (gatts-make-char-value +gatt-uuid-gap-appearance+ (list :read (lambda (c h)
                                                                   (declare (ignore c h))
                                                                   (make-c-int :u16 #x0012))))

   (gatts-make-service +gatt-uuid-gatt-service+)
   (gatts-make-char-decl +gatt-uuid-gatt-service-changed+ (make-props '(:indicate)))
   (gatts-make-char-value +gatt-uuid-gatt-service-changed+ '())
   (gatts-make-cccd (make-cccd-storage))

   (gatts-make-service +gatt-uuid-heart-rate-service+)
   (gatts-make-char-decl +gatt-uuid-heart-rate-measurement+ (make-props '(:read :notify)))
   (gatts-make-char-value +gatt-uuid-heart-rate-measurement+ (list :read #'read-spy))
   (gatts-make-cccd (make-cccd-storage))

   ;; Dummy nordic uart service to test out 128-bit discovery
   (gatts-make-service #x6E400001B5A3F393E0A9E50E24DCCA9E)
   (gatts-make-char-decl #x6E400003B5A3F393E0A9E50E24DCCA9E (make-props '(:read :notify)))
   (gatts-make-char-value #x6E400003B5A3F393E0A9E50E24DCCA9E (list :read #'read-spy))
   (gatts-make-cccd (make-cccd-storage))
   (gatts-make-char-decl #x6E400002B5A3F393E0A9E50E24DCCA9E (make-props '(:write :write-no-rsp)))
   (gatts-make-char-value #x6E400002B5A3F393E0A9E50E24DCCA9E (list :write #'write-spy))
   ))

(defun read-cccd (conn table value-handle)
  (let* ((cccd-handle
          (gattc-find-cccd-handle table +gatt-uuid-cccd+ value-handle))
         (cccd-value (when cccd-handle
                       (funcall
                        (getf (nth (- cccd-handle 1) table) :read)
                        conn
                        cccd-handle))))
    (unless (listp cccd-value)
      (log-err (format nil "CCCD error: ~A" cccd-value)))
    (when (listp cccd-value)
      cccd-value)))

(gattc-print *gatts-table*)
;  => "
; ...1 SERVICE 2800
; ...2   CHARACTERISTIC (PROP 12) (UUID 2803)
; ...3     VALUE
; ...4     CCCD
; "

(defun att-make-error (code)
  (getf +att-errors+ code))

(defun att-error-rsp (opcode handle code)
  (att-make-packet :error-rsp
                   (append
                    (make-c-int :u8 (att-make-opcode opcode t))
                    (make-c-int :u16 handle)
                    (make-c-int :u8 (att-make-error code)))))

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

;; (gatt-find-service
;;  *gatts-table* +gatt-uuid-heart-rate-service+ 1 #xFFFF)
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
         (uuid (decode-c-int (pull req (length req)))))
    (if (eql type +gatt-uuid-primary-service+)
        (gatts-find-service-rsp *gatts-table* uuid start end)
        (att-error-rsp
         :find-by-type-value-req 0 :request-not-supported))))

(defun gatts-find-char-rsp (table uuid search-start search-end)
  (let* ((handle
           (gatt-find-handle table uuid
                             :start search-start :end search-end)))
    (if handle
        (let* ((read-fn
                 (getf (nth (- handle 1) table) :read))
               (data (funcall read-fn 0 handle)))
          (when (> (length data) (- #xFF 2))
            (error "aiight gotta implement this now"))
          (att-make-packet :read-by-type-rsp
                           (append
                            (make-c-int :u8 (+ 2 (length data)))
                            (make-c-int :u16 handle)
                            data)))
        (att-error-rsp :read-by-type-req
                       search-start :attribute-not-found))))

(defun gatts-process-read-by-type (conn req)
  (declare (ignore conn))
  (let* ((start (pull-int req :u16))
         (end (pull-int req :u16))
         (type (pull-int req :u16)))
    (log-dbg (format nil "READ-BY-TYPE-REQ start ~X end ~X" start end))
    (gatts-find-char-rsp *gatts-table* type start end)))

(defun get-uuid-size (uuid)
  ;; Here uuid is a number, not a list
  (if (< uuid #x10000) 2 16))

(get-uuid-size #x1230)
 ; => 2 (2 bits, #x2, #o2, #b10)
(get-uuid-size #x6E400001B5A3F393E0A9E50E24DCCA9E)
 ; => 16 (5 bits, #x10, #o20, #b10000)

(defun gatts-read-by-group-type-rsp (table search-start search-end)
  (destructuring-bind (&key start end)
      (gatt-find-service table nil search-start search-end)
    (if start
        (let* ((read-fn (getf (nth (- start 1) table) :read))
               (svc-uuid (decode-c-int (funcall read-fn 0 start)))
               (uuid-size (get-uuid-size svc-uuid)))
          (att-make-packet :read-by-group-type-rsp
                           (append
                            (make-c-int :u8 (+ 2 2 uuid-size))
                            (make-c-int :u16 start)
                            (make-c-int :u16 end)
                            (make-uint uuid-size svc-uuid))))
        (att-error-rsp :read-by-group-type-req
                       0 :attribute-not-found))))

(defun gatts-process-read-by-group-type (conn req)
  (declare (ignore conn))
  (let* ((start (pull-int req :u16))
         (end (pull-int req :u16))
         (type (pull-int req :u16)))
    (log-dbg (format nil "READ-BY-GROUP-TYPE-REQ start ~X end ~X" start end))
    (if (eql type +gatt-uuid-primary-service+)
        (gatts-read-by-group-type-rsp *gatts-table* start end)
        (att-error-rsp
         :read-by-group-type-req 0 :request-not-supported))))

(defun gatts-find-info-rsp (table start end)
  (let* ((uuid-size)
         (terminate nil)
         (information-data
           (when (>= (length table) start)
             (apply #'nconc
                    (mapcar
                     (lambda (a)
                       (unless terminate
                         (unless uuid-size
                           (setf uuid-size (get-uuid-size (getf a :uuid))))
                         (unless (= uuid-size (get-uuid-size (getf a :uuid)))
                           (setf terminate t))
                         (unless terminate
                             (append (make-c-int :u16 (getf a :handle))
                                     (make-uint uuid-size (getf a :uuid))))))
                     (subseq table
                             (- (min (length table) start) 1)
                             (min (length table) end)))))))
    (if information-data
        (att-make-packet :find-information-rsp
                         (append
                          (make-c-int :u8
                                      (if (= uuid-size 2) #x01 #x02))
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
    (log-dbg (format nil "FIND-INFO-REQ start ~X end ~X" start end))
    (gatts-find-info-rsp *gatts-table* start end)))

(defun gatts-process-write (conn req)
  (let* ((handle (pull-int req :u16))
         (rsp (funcall
               (getf (nth (- handle 1) *gatts-table*) :write)
               conn handle req)))
    (log-dbg (format nil "WRITE-REQ handle ~X" handle))
    (if rsp
        (att-make-packet :error-rsp
                         (append
                          (att-make-opcode :write-req)
                          (make-c-int :u16 handle)
                          (make-c-int :u8 (att-make-error rsp))))
        (att-make-packet :write-rsp nil))))

(defun gatts-process-read (conn req)
  (let* ((handle (pull-int req :u16))
         (rsp (funcall
               (getf (nth (- handle 1) *gatts-table*) :read)
               conn handle)))
    (log-dbg (format nil "READ-REQ handle ~X" handle))
    (if (listp rsp)
        (att-make-packet :read-rsp rsp)
        (att-make-packet :error-rsp
                         (append
                          (att-make-opcode :read-req)
                          (make-c-int :u16 handle)
                          (make-c-int :u8 (att-make-error rsp)))))))

(defun gattc-process-notification (conn req)
  (declare (ignore conn))
  (log-dbg (format nil "Notification: handle ~X value ~X"
                   (pull-int req :u16) req))
  ;; Don't reply to notifications
  nil)

(defun gatts-process-mtu-req (conn req)
  (declare (ignore conn))
  (let ((mtu (pull-int req :u16)))
    (log-dbg "MTU req: ~A" mtu)
    (att-make-packet :exchange-mtu-rsp
                     (make-c-int :u16 mtu))))

(defparameter *mitm* nil)
(defun start-mitm (central-conn peripheral-conn)
  (setf *mitm*
      (list
       :central central-conn
       :peripheral peripheral-conn
       :ready nil)))

(defparameter *mitm-att-queue* '())
(defparameter *mitm-signalling-queue* '())
(defun disable-mitm ()
  (setf *mitm-att-queue* nil)
  (setf *mitm-signalling-queue* nil)
  (setf *mitm* nil))

(defun arm-mitm ()
  (setf *mitm* (list :armed t)))

(defun mitm-other-conn (conn)
  (if (equal conn (getf *mitm* :central))
      (getf *mitm* :peripheral)
      (getf *mitm* :central)))

(defun handle-att-mitm (hci conn req)
  (if (not (getf *mitm* :ready))
      (push (list :conn conn :req req) *mitm-att-queue*)
      (progn
        (log-dbg (format nil "MITM [~A -> ~A] ~X"
                         conn (mitm-other-conn conn) req))
        (att-send hci (mitm-other-conn conn) req))))

(defun drain-mitm-att-queue (hci)
  (setf (getf *mitm* :ready) t)
  (mapcar (lambda (packet)
            (handle-att-mitm hci
                             (getf packet :conn)
                             (getf packet :req)))
          *mitm-att-queue*)
  (setf *mitm-att-queue* nil))

(defun handle-att (hci conn req)
  (if *mitm*
      (handle-att-mitm hci conn req)
      (let* ((op (pull-int req :u8))
             (op-name (plist-key +att-opcodes+ op)))
        ;; TODO: log conn in ATT/SMP RX/TX
        (log-dbg (format nil "ATT-RX: OP ~X DATA ~X" op-name req))
        (att-send
         hci conn
         (case op-name
           (:exchange-mtu-req
            (gatts-process-mtu-req conn req))
           (:find-by-type-value-req
            (gatts-process-find-by-type-value conn req))
           (:read-by-type-req
            (gatts-process-read-by-type conn req))
           (:find-information-req
            (gatts-process-find-information conn req))
           (:write-req
            (gatts-process-write conn req))
           (:read-req
            (gatts-process-read conn req))
           (:read-by-group-type-req
            (gatts-process-read-by-group-type conn req))
           (:handle-value-ntf
            (gattc-process-notification conn req))
           (t
            (att-error-rsp op-name 0 :request-not-supported)))))))

(defun wait-for-att-request (hci conn)
  ;; Wait for the next ATT request
  (handle-att
   hci conn
   (getf (receive-if hci (att? conn nil t)) :data)))

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

(defun name? (name report)
  ;; Sample report
  (let* ((ad (getf (copy-tree report) :data))
         (parsed (parse-ad ad))
         (encoded-name (getf parsed :name-complete)))
    (log-dbg (format nil "search ~A in: ~A" name encoded-name))
    (if encoded-name
        (search name (from-c-string encoded-name) :test #'equalp)
        nil)))

(defun uuid? (uuid report)
  ;; Sample report
  (let* ((ad (getf (copy-tree report) :data))
         (parsed (parse-ad ad))
         (uuids (or (getf parsed :class-uuid-16-complete)
                    (getf parsed :class-uuid-16-incomplete))))
    (search (make-c-int :u16 uuid) uuids)))

(defparameter +smp-opcodes+
  (list :pairing-request #x01
        :pairing-response #x02
        :pairing-confirm #x03
        :pairing-random #x04
        :pairing-failed #x05
        :encryption-information #x06
        :central-identification #x07
        :identity-information #x08
        :identity-address-information #x09
        :signing-information #x0A
        :security-request #x0B
        :pairing-public-key #x0C
        :pairing-dhkey-check #x0D
        :pairing-keypress-notification #x0E))

(defparameter +l2cap-smp-chan+ #x0006)

(defun smp? (conn-handle)
  (lambda (p)
    ;; sample p: (ACL (CONN-HANDLE 0 LENGTH 3 CHANNEL 4 DATA (3 40 1)))
    (and (eql (car p) :acl)
         (eql (getf (cadr p) :conn-handle)
              conn-handle)
         (eql (getf (cadr p) :channel)
              +l2cap-smp-chan+))))

(defun smp-receive (hci conn-handle)
  (receive-if hci (smp? conn-handle)))

(defun smp-make-opcode (op-name &optional single)
  (if (not single)
      (make-c-int :u8 (getf +smp-opcodes+ op-name))
      (getf +smp-opcodes+ op-name)))

(defun smp-make-packet (op param)
  (append (smp-make-opcode op) param))

(defparameter *testkey* nil)
;; (defparameter *testkey* (ironclad:generate-key-pair :SECP256R1))

(defun smp-make-privkey ()
  (if *testkey*
      *testkey*
      (ironclad:generate-key-pair :SECP256R1)))

;; (pxx
;;  (coerce (getf
;;           (ironclad:destructure-private-key *testkey*) :x) 'list))
;;  ; => "0xC4DE2220375311EFEB2AEDDE611D8B2B59D8AFD7F3339BF1F34D682FE03ED5E0"

(defun smp->ironclad (pubkey)
  (ironclad:make-public-key
   :SECP256R1
   :Y
   (coerce
    (let* ((x (reverse (subseq pubkey 0 32)))
           (y (reverse (subseq pubkey 32 64)))
           (be (append (list #x04) x y)))
      be)
    '(vector (unsigned-byte 8)))))

(defun ironclad->smp (ic-privkey)
  (let* ((serialized (ironclad:destructure-private-key ic-privkey))
         (pubkey (coerce (getf serialized :Y) 'list))
         (x (reverse (subseq pubkey 1 33)))
         (y (reverse (subseq pubkey 33 65)))
         (le-pubkey (append x y)))
    le-pubkey))

(defun smp-get-privkey (conn)
  (ironclad->smp (getf (get-smp-context conn) :our-privkey)))

(defun smp-dhkey (conn)
  (let* ((priv (getf (get-smp-context conn) :our-privkey))
         (pub (smp->ironclad (getf (get-smp-context conn) :peer-pubkey))))
    (log-dbg (format nil "SMP: priv ~X" priv))
    (log-dbg (format nil "SMP: pub ~X" (getf (get-smp-context conn) :peer-pubkey)))
    (setf
     (getf (get-smp-context conn) :dhkey)
     (reverse
      (subseq
       (coerce
        (ironclad:diffie-hellman priv pub)
        'list) 1 33)))))

(defun as-array (l)
  (coerce l '(vector (unsigned-byte 8))))

(defun smp-cmac (key &rest texts)
  (reverse
   (coerce
    (ironclad:with-authenticating-stream (s :cmac (as-array (reverse key)) :aes)
      (mapcar (lambda (e) (write-bytes (reverse e) s))
              texts))
    'list)))

(defparameter +iocap-no-display-no-keyboard+ #x03)

(defparameter +our-iocap+ '(#x03 #x00 #x09))

(defun smp-send (hci conn-handle payload)
  (when payload
    ;; (log-dbg (format nil "SMP-TX ~X" payload))
    (l2cap-send hci conn-handle +l2cap-smp-chan+ payload)))

(defun smp-send-security-req (hci conn)
  (smp-send
   hci conn (smp-make-packet
             :security-request
             (make-c-int :u8 #x09))))

(defun smp-send-pairing-req (hci conn)
  (let* ((iocap +iocap-no-display-no-keyboard+)
         (oob-flag #x00)                ; no OOB
         (authreq #x09)                 ; LESC, bonding
         (max-key-size 16)
         (ini-key-dist #x01)            ; "distribute" LTK
         (rsp-key-dist #x01))

    ;; [v5.4 p1557] ok so here's the gotcha: iocap in the PDU description IS NOT
    ;; the same IOcap as used in the f6 function. There it means
    ;; iocap+oob+authreq, so we save that instead.
    ;; (unless (equal +our-iocap+ (list iocap oob-flag authreq))
    ;;   (error "IOcap not supported yet"))

    ;; Reset the SMP context upon sending Pairing Request
    (setf (get-smp-context conn)
          (list :random-central (make-list 16 :initial-element 99) ; technically random
                :our-privkey (smp-make-privkey)))

    (smp-send
     hci conn
     (smp-make-packet
      :pairing-request
      (list iocap oob-flag authreq
            max-key-size ini-key-dist rsp-key-dist)))))

(defun smp-process-pairing-rsp (conn data)
  (let* ((iocap (pull-int data :u8))
         (oob-flag (pull-int data :u8))
         (authreq (pull-int data :u8)))

    (setf (getf (get-smp-context conn) :iocap) (list iocap oob-flag authreq))
    (setf (getf (get-smp-context conn) :initiated-by-us) t)

    ;; Next step is public key
    (smp-make-packet :pairing-public-key (smp-get-privkey conn))))

(defun smp-process-pairing-req (conn data)
  (let* ((iocap (pull-int data :u8))
         (oob-flag (pull-int data :u8))
         (authreq (pull-int data :u8))
         (max-key-size (pull-int data :u8))
         (ini-key-dist (pull-int data :u8))
         (rsp-key-dist (pull-int data :u8)))
    (declare (ignore max-key-size rsp-key-dist))

    ;; [v5.4 p1557] ok so here's the gotcha: iocap in the PDU description IS NOT
    ;; the same IOcap as used in the f6 function. There it means
    ;; iocap+oob+authreq, so we save that instead.
    ;; (unless (equal +our-iocap+ (list iocap oob-flag authreq))
    ;;   (error "IOcap not supported yet"))

    ;; Reset the SMP context upon receiving Pairing Request
    (setf (get-smp-context conn)
          (list :iocap (list iocap oob-flag authreq)
                :random-peripheral (make-list 16 :initial-element 77) ; technically random
                :our-privkey (smp-make-privkey)))

    (smp-make-packet
     :pairing-response
     (list
      +iocap-no-display-no-keyboard+
      #x00                              ; no OOB
      #x09                              ; LESC, bonding
      16                                ; max keysize
      ini-key-dist
      #x01                              ; rsp keydist: LTK only
      ))
    ))

(defun smp-process-public-key (conn data)
  (let* ((peer-pubkey data)
         (our-pubkey-le (smp-get-privkey conn)))

    ;; (log-dbg (format nil "SMP: peer pubkey ~X" peer-pubkey))
    (setf (getf (get-smp-context conn) :peer-pubkey) peer-pubkey)

    (unless (= (length our-pubkey-le) 64)
      (error "key conversion error"))

    ;; If we're a peripheral, we want to send our public key
    ;; If we're a central, we wait for the pairing confirm

    (if (getf (get-smp-context conn) :initiated-by-us)
        nil
        (smp-make-packet :pairing-public-key
                         our-pubkey-le))))

(defun smp-get-our-pubkey (conn)
  (subseq (smp-get-privkey conn) 0 32))

(defun smp-f4 (U V X Z)
  (smp-cmac X U V Z))

(defun smp-send-pairing-confirm (hci conn)
  ;; Only sent as non-initiating device (peripheral)
  (smp-send
   hci conn
   (smp-make-packet
    :pairing-confirm
    (let* ((nb (getf (get-smp-context conn) :random-peripheral))
           (pk-b-x (subseq (smp-get-our-pubkey conn) 0 32))
           (pk-a-x (subseq (getf (get-smp-context conn) :peer-pubkey) 0 32))
           (confirm-b (smp-f4 pk-b-x pk-a-x nb (make-c-int :u8 0))))
      ;; (log-dbg (format nil "SMP confirm-b ~X" confirm-b))
      confirm-b))))

(defun smp-process-confirm (conn data)
  ;; let's just ignore the confirm for now
  (declare (ignore data))
  ;; next step is sending our random
  (smp-make-packet :pairing-random
                   (getf (get-smp-context conn) :random-central)))

(defun smp-process-random (conn data)
  (let* ((peer-random data))

    ;; (log-dbg (format nil "SMP: peer random ~X" peer-random))
    (if (getf (get-smp-context conn) :initiated-by-us)
        (progn
          (setf (getf (get-smp-context conn) :random-peripheral) peer-random)
          nil)
        (progn
          (setf (getf (get-smp-context conn) :random-central) peer-random)
          (smp-make-packet :pairing-random
                           (getf (get-smp-context conn) :random-peripheral))))))

(defun smp-f6 (W N1 N2 R IOcap A1 A2)
  (log-dbg (format nil "SMP: f6: W ~X" W))
  (log-dbg (format nil "SMP: f6: N1 ~X" N1))
  (log-dbg (format nil "SMP: f6: N2 ~X" N2))
  (log-dbg (format nil "SMP: f6: A1 ~X" A1))
  (log-dbg (format nil "SMP: f6: A2 ~X" A2))
  (log-dbg (format nil "SMP: f6: R ~X" R))
  (log-dbg (format nil "SMP: f6: iocap ~X" IOcap))
  (smp-cmac W N1 N2 R IOcap A1 A2))

(defun smp-addr (conn &key peer)
  (let ((address (getf
                  (getf *active-conns* conn)
                  (if peer :address :our-address))))
    (append
     (make-uint 6 (getf address :address))
     (make-uint 1 (logand (getf address :type) #x01)))))

(defun smp-addr-central (conn)
  (if (getf (get-smp-context conn) :initiated-by-us)
      (smp-addr conn)
      (smp-addr conn :peer t)))

(defun smp-addr-peripheral (conn)
  (if (getf (get-smp-context conn) :initiated-by-us)
      (smp-addr conn :peer t)
      (smp-addr conn)))

(defun smp-compute-dhkey-check (conn &key Ea)
  ;; [v5.4 p1556] Calculate Ea/Eb
  (let* ((mackey (getf (get-smp-context conn) :mackey))
         (Na (getf (get-smp-context conn) :random-central))
         (Nb (getf (get-smp-context conn) :random-peripheral))
         (r (make-list 16 :initial-element 0))
         (IOcap +our-iocap+)            ; it's always our iocap
         (addr-a-c (smp-addr-central conn))
         (addr-b-p (smp-addr-peripheral conn)))
    (if Ea
        (smp-f6 mackey Na Nb r IOcap addr-a-c addr-b-p)
        (smp-f6 mackey Nb Na r IOcap addr-b-p addr-a-c))))

(defun smp-process-dhkey-check (conn data)
  (let* ((peer-dhkey-check data)
         (dhkey-check-Eb
           (smp-compute-dhkey-check conn)))

    ;; (log-dbg (format nil "SMP: peer DHKey check ~X" peer-dhkey-check))
    (setf (getf (get-smp-context conn) :peer-dhkey-check) peer-dhkey-check)

    ;; (log-dbg (format nil "SMP: our DHKey check ~X" dhkey-check-Eb))
    (setf (getf (get-smp-context conn) :our-dhkey-check) dhkey-check-Eb)

    ;; TODO: verify dhkey. security lol
    (when (not (getf (get-smp-context conn) :initiated-by-us))
      (smp-make-packet :pairing-dhkey-check
                       dhkey-check-Eb))))

(defun smp-f5 (W N1 N2 A1 A2)
  ;; [v5.4 p1555 p1577]
  (let* ((salt (reverse '(#x6c #x88 #x83 #x91 #xaa #xf5 #xa5 #x38
                          #x60 #x37 #x0b #xdb #x5a #x60 #x83 #xbe)))
         (key-T (smp-cmac salt W))

         (keyid (reverse (to-c-string "btle")))
         (mackey (smp-cmac key-T
                           (make-c-int :u8 0)
                           keyid
                           N1
                           N2
                           A1
                           A2
                           (make-c-int :u16 256)))
         (ltk (smp-cmac key-T
                        (make-c-int :u8 1)
                        keyid
                        N1
                        N2
                        A1
                        A2
                        (make-c-int :u16 256))))

    (log-dbg (format nil "SMP: f5: T ~X" key-T))
    (log-dbg (format nil "SMP: f5: N1 ~X" N1))
    (log-dbg (format nil "SMP: f5: N2 ~X" N2))
    (log-dbg (format nil "SMP: f5: A1 ~X" A1))
    (log-dbg (format nil "SMP: f5: A2 ~X" A2))
    (log-dbg (format nil "SMP: f5: mc ~X" mackey))
    (log-dbg (format nil "SMP: f5: ltk ~X" ltk))

    (append mackey ltk)))

(defun smp-compute-and-store-ltk (conn)
  (let* ((Nb (getf (get-smp-context conn) :random-peripheral))
         (Na (getf (get-smp-context conn) :random-central))
         (addr-a-c (smp-addr-central conn))
         (addr-b-p (smp-addr-peripheral conn))
         (dhkey (smp-dhkey conn))
         (f5-out (smp-f5 dhkey Na Nb addr-a-c addr-b-p))
         (mackey (subseq f5-out 0 16))
         (ltk (subseq f5-out 16 32)))
    (log-dbg (format nil "SMP: dhkey ~X" dhkey))
    (log-dbg (format nil "SMP: ltk ~X" ltk))
    (log-dbg (format nil "SMP: mackey ~X" mackey))
    (setf (getf (get-smp-context conn) :ltk) ltk)
    (setf (getf (get-smp-context conn) :mackey) mackey)))

(defun enc-change? (evt-code)
  (lambda (packet)
    (eql (car (cadr packet)) evt-code)))

(defun wait-for-encryption (hci conn)
  (receive-if hci
              (lambda (packet)
                (and
                 (eql (car (cadr packet)) :encryption-change)
                 (eql (getf (cadr (cadr packet)) :handle) conn))))
  (setf (getf (get-smp-context conn) :encrypted) t))

(defun provide-ltk (hci conn ltk)
  (if ltk
      (progn
        (log-dbg (format nil "ENCRYPTION: providing LTK ~X" ltk))
        (hci-send-cmd
         hci
         (make-hci-cmd :le-ltk-request-reply
                       :handle conn
                       :ltk ltk)))
      (progn
        (log-dbg (format nil "ENCRYPTION: NO LTK"))
        (hci-send-cmd
         hci
         (make-hci-cmd :le-ltk-request-neg-reply
                       :handle conn)))))

(defun bond-address (bond)
  (getf
   (getf bond :peer)
   ;; some day I'll use proper objects
   :address))

(defparameter *bonds* (make-hash-table))
(defun store-bond (bond)
  (setf
   (gethash (bond-address bond) *bonds*)
   bond))

(defun bonds->list (bonds-hashmap)
  (let ((bonds-list))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v bonds-list))
             bonds-hashmap)
    bonds-list))

(defun make-bond (address ltk)
  (list :peer address
        :ltk ltk))

(defun is-bonded (address)
  (gethash address *bonds*))

(defun get-ltk (conn)
  (getf
   (gethash (get-address conn) *bonds*)
   :ltk))

(defun process-ltk-request (hci evt)
  (let* ((conn (getf (cadr evt) :conn-handle))
         (ltk (or (getf (get-smp-context conn) :ltk)
                  (get-ltk conn))))
    (provide-ltk hci conn ltk)))

(defun smp-process-security-req (conn data)
  (declare (ignore conn data))
  nil)

(defun smp-process-pairing-failed (conn data)
  (declare (ignore data))
  (log-inf (format nil "PAIRING FAILED (conn ~A)" conn))
  nil)

(defun smp-send-dhkey-check-ea (hci conn)
  (let* ((dhkey-check-Ea (smp-compute-dhkey-check conn :Ea t)))
    (smp-send hci conn
              (smp-make-packet :pairing-dhkey-check
                               dhkey-check-Ea))))

(defun handle-smp (hci conn packet)
  (let* ((data (getf packet :data))
         (opcode (pull-int data :u8))
         (op-name (plist-key +smp-opcodes+ opcode)))
    (log-dbg (format nil "SMP-RX: OP ~X DATA ~X" op-name data))
    (smp-send
     hci conn
     (ecase op-name
       (:identity-address-information nil)
       (:identity-information nil)
       (:pairing-failed
        (smp-process-pairing-failed conn data))
       (:security-request
        (smp-process-security-req conn data))
       (:pairing-request
        (smp-process-pairing-req conn data))
       (:pairing-response
        (smp-process-pairing-rsp conn data))
       (:pairing-public-key
        (smp-process-public-key conn data))
       (:pairing-confirm
        (smp-process-confirm conn data))
       (:pairing-random
        (smp-process-random conn data))
       (:pairing-dhkey-check
        (smp-process-dhkey-check conn data))
       ))

    (unless
        (getf (get-smp-context conn) :sent-confirm)
      (when (and
             (not (getf (get-smp-context conn) :initiated-by-us))
             (getf (get-smp-context conn) :peer-pubkey))
        (setf (getf (get-smp-context conn) :sent-confirm) t)
        (smp-send-pairing-confirm hci conn)))

    (unless (getf (get-smp-context conn) :ltk)
      (when (and
             (getf (get-smp-context conn) :random-peripheral)
             (getf (get-smp-context conn) :random-central))
        (smp-compute-and-store-ltk conn)

        (when (getf (get-smp-context conn) :initiated-by-us)
          (smp-send-dhkey-check-ea hci conn))))

    (unless (getf (get-smp-context conn) :stored-bond)
      (when (getf (get-smp-context conn) :peer-dhkey-check)
        (setf (getf (get-smp-context conn) :stored-bond) t)
        (store-bond
         (make-bond
          (getf (getf *active-conns* conn) :address)
          (getf (get-smp-context conn) :ltk)))

        ;; If we're the central, we should start encryption now
        (when (getf (get-smp-context conn) :initiated-by-us)
          (hci-send-cmd
           hci
           (make-hci-cmd :le-enable-encryption
                         :handle conn
                         :rand 0
                         :ediv 0
                         :ltk (getf (get-smp-context conn) :ltk))))))
    ))

(defun encrypt-link (hci conn)
  (let ((ltk (if (is-bonded (get-address conn))
                 (get-ltk conn)
                 (getf (get-smp-context conn) :ltk))))
    (log-inf (format nil "Encrypting with LTK ~X" ltk))
    (hci-send-cmd
     hci
     (make-hci-cmd :le-enable-encryption
                   :handle conn
                   :rand 0
                   :ediv 0
                   :ltk ltk))
    ltk))

(defparameter +l2cap-le-signalling-chan+ #x0005)
(defparameter +l2cap-conn-param-update-req+ #x12)
(defparameter +l2cap-conn-param-update-rsp+ #x13)

(defun handle-signalling-mitm (hci conn data)
  (if (not (getf *mitm* :ready))
      (push (list :conn conn :data data) *mitm-signalling-queue*)
      (progn
        (log-inf (format nil "SIGNALLING PACKET: conn ~A data ~X" conn data))
        (log-dbg (format nil "MITM [~A -> ~A] ~X"
                         conn (mitm-other-conn conn) data))
        (l2cap-send hci (mitm-other-conn conn) +l2cap-le-signalling-chan+
                    (getf data :data)))))

(defun handle-signalling (hci conn data)
  (if *mitm*
      (handle-signalling-mitm hci conn data)
      (let* ((payload (getf data :data))
             (opcode (pull-int payload :u8))
             (identifier (pull-int payload :u8)))
        (log-inf (format nil "SIGNALLING PACKET: conn ~A op ~X payload ~X" opcode conn payload))
        (l2cap-send hci conn +l2cap-le-signalling-chan+
                    (cond
                      ((= opcode +l2cap-conn-param-update-req+)
                       (append
                        (make-c-int :u8 +l2cap-conn-param-update-rsp+)
                        (make-c-int :u8 identifier)
                        (make-c-int :u16 #x2)
                        (make-c-int :u16 #x0001)))))))) ; rejecc

(defun drain-mitm-signalling-queue (hci)
  (setf (getf *mitm* :ready) t)
  (mapcar (lambda (packet)
            (handle-signalling hci
                               (getf packet :conn)
                               (getf packet :data)))
          *mitm-signalling-queue*)
  (setf *mitm-signalling-queue* nil))

(defun handle-acl (hci packet)
  ;; For some weird reason (probably pebcak) CASE doesn't work with constants.
  (cond
    ((eql (getf packet :channel) +l2cap-att-chan+)
     (handle-att hci (getf packet :conn-handle) (getf packet :data)))
    ((eql (getf packet :channel) +l2cap-smp-chan+)
     (handle-smp hci (getf packet :conn-handle) packet))
    ((eql (getf packet :channel) +l2cap-le-signalling-chan+)
     (handle-signalling hci (getf packet :conn-handle) packet))
    (t (error "Unknown l2cap channel"))
    ))

(defun decode-att-data (op data)
  (if (eq-any op :write-req :write-cmd :read-req :handle-value-ntf)
   (list :handle (pull-int data :u16)
         :data data)
   (list :data data)))

(defun decode-att (packet)
  (when (eql +l2cap-att-chan+ (getf packet :channel))
    (let* ((op (plist-key +att-opcodes+ (pull-int (getf packet :data) :u8)))
           (conn (getf packet :conn-handle))
           (data (getf packet :data))
           (decoded (decode-att-data op data)))
      (append
       (list :conn conn :op op)
       decoded))))

(defun process-remote-conn-param (hci packet)
  (log-dbg (format nil "NAK remote conn param ~X" packet))
  (hci-send-cmd hci (make-hci-cmd :le-remote-conn-param-req-neg-reply
                                  :handle (getf (cadr packet) :conn-handle)
                                  :reason #x3B)))

(defun process-encryption-change (hci packet)
  (declare (ignore hci))
  (let ((conn (getf (cadr packet) :handle)))
    (setf (getf (get-smp-context conn) :encrypted) t)))

(defun process-le-conn-update-complete (hci packet)
  (destructuring-bind (&key conn-handle interval latency timeout
                         &allow-other-keys)
      (cadr packet)
    (when (and (getf *mitm* :ready)
               (getf *mitm* :central)
               (eql (getf *mitm* :central) conn-handle))
      (log-inf "MITM: forwarding conn update")
      (hci-send-cmd
       hci
       (make-hci-cmd :le-conn-update
                     :handle (mitm-other-conn conn-handle)
                     :interval-min interval
                     :interval-max interval
                     :max-latency latency
                     :timeout timeout
                     :min-ce-len 0
                     :max-ce-len (- (* interval 2) 10))))))

(defun handle-evt (hci packet)
  ;; TODO: don't /dev/null the 'vents
  (log-dbg (format nil "HANDLE-EVT ~X" packet))
  (case (car packet)
    (:le-remote-conn-param-req
     (process-remote-conn-param hci packet))
    (:le-ltk-request
     (process-ltk-request hci packet))
    (:encryption-change
     (process-encryption-change hci packet))
    (:le-conn-update
     (process-le-conn-update-complete hci packet))
     ))

(defun process-hci (hci packet)
  (log-trace (format nil "PROCESS-HCI ~X" packet))
  (cond
    ((eql (car packet) :acl)
     (handle-acl hci (cadr packet)))
    ((eql (car packet) :evt)
     (handle-evt hci (cadr packet)))
    (t (error "Unknown packet"))))

(defun init-controller (hci)
  (hci-reset hci)
  (hci-read-buffer-size hci)
  (hci-allow-all-the-events hci)
  (hci-set-random-address hci #xC1234567890A))

(defun start-advertising (hci adv-data)
  (hci-set-adv-param hci)
  (hci-set-adv-data hci adv-data)
  (hci-set-adv-enable t hci))

(defun start-security (hci conn &key is-peripheral)
  (if is-peripheral
      (smp-send-security-req hci conn)
      (if (is-bonded (get-address conn))
          (encrypt-link hci conn)
          (smp-send-pairing-req hci conn))))

(defun monitor-thread (stop-signal)
  (bt:make-thread
   (lambda () (wait-for-signal stop-signal))
   :name "Server signal thread"))

(defun packetizer-master-thread (packetizer stop-signal)
  (let ((threads))
    (unwind-protect
         (progn
           (push (receive-in-thread packetizer) threads)
           (push (send-in-thread packetizer) threads)
           (push (monitor-thread stop-signal) threads)
           (ignore-errors
            (loop until (find-if-not #'bt:thread-alive-p threads)
                  do (sleep .5))))
      (progn
        (log-dbg (format nil "Killing threads: ~A" threads))
        (mapc
         (lambda (th) (ignore-errors
                       (bt:destroy-thread th)))
         threads)))
    ))

(defun start-hci (packetizer-path h2c-mailbox c2h-mailbox stop-signal)
  ;; First thing is to empty the mailboxes
  (loop while (drain-mailbox h2c-mailbox))
  (loop while (drain-mailbox c2h-mailbox))

  (bt:make-thread
   (lambda ()
     (if (listp packetizer-path)
         (with-fifo-packetizer
             packetizer
             (getf packetizer-path :h2c)
             (getf packetizer-path :c2h)
           (setf (getf packetizer :rx-mailbox) c2h-mailbox)
           (setf (getf packetizer :tx-mailbox) h2c-mailbox)
           (packetizer-master-thread packetizer stop-signal))

         (with-serial-packetizer packetizer packetizer-path
           (setf (getf packetizer :rx-mailbox) c2h-mailbox)
           (setf (getf packetizer :tx-mailbox) h2c-mailbox)
           (packetizer-master-thread packetizer stop-signal))))
   :name "Packetizer (HCI server) master thread"))

(defun stop-hci (stop-signal)
  ;; Kill all threads started by start-hci
  (send-signal stop-signal))
