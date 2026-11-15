(require 'asdf)
(require :bordeaux-threads)
(require :cserial-port)
(require :local-time)
(require :sb-concurrency)
(require :sb-posix)

;;;;;;;;;;;;; package
;; Packages don't exist in ulisp so we put it in platform

(defpackage :host
  (:use :common-lisp))

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

;;;;;;;;;;;;; HCI drivers

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

;;;;;;;;;;;;; strings & chars

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

;;;;;;;;;;;;; Concurrency

(defun make-signal ()
  (sb-concurrency:make-gate))

(defun wait-for-signal (var)
  (sb-concurrency:wait-on-gate var))

(defun send-signal (var)
  (sb-concurrency:open-gate var))

(defun make-mailbox (name)
  (sb-concurrency:make-mailbox :name name))

(defun receive-from-mailbox (mb &optional (timeout 0.1))
  (sb-concurrency:receive-message mb :timeout timeout))

(defun send-to-mailbox (mb message)
  (sb-concurrency:send-message mb message))

(defun empty-mailbox-p (mb)
  (sb-concurrency:mailbox-empty-p mb))
