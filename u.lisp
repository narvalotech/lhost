(defpackage #:ulisp
  (:use) ; Intentionally empty: does NOT inherit the full CL package
  (:import-from #:cl
               ;; Core & Special Forms
               #:car #:cdr #:cons #:list #:quote #:defun #:defvar #:setq
               #:let #:let* #:lambda #:if #:cond #:when #:unless #:progn
               #:loop #:return #:and #:or #:not #:eq #:equal

               ;; Type Checkers
               #:atom #:listp #:consp #:null #:symbolp #:numberp #:stringp

               ;; List & Sequence Operations
               #:append #:reverse #:member #:assoc #:nth #:nthcdr #:last
               #:mapcar #:apply #:length #:elt #:subseq #:concatenate #:string=

               ;; Arithmetic & Comparisons
               #:+ #-#:- #:* #:/ #:1+ #:1- #:rem #:abs #:min #:max #:sqrt #:random
               #:= #:/= #:< #:<= #:> #:>=

               ;; Basic I/O & Evaluation
               #:read #:print #:prin1 #:princ #:terpri #:eval)
  (:export
   ;; Export everything imported so users of the package can use them
   #:car #:cdr #:cons #:list #:quote #:defun #:defvar #:setq
   #:let #:let* #:lambda #:if #:cond #:when #:unless #:progn
   #:loop #:return #:and #:or #:not #:eq #:equal
   #:atom #:listp #:consp #:null #:symbolp #:numberp #:stringp
   #:append #:reverse #:member #:assoc #:nth #:nthcdr #:last
   #:mapcar #:apply #:length #:elt #:subseq #:concatenate #:string=
   #:+ #-#:- #:* #:/ #:1+ #:1- #:rem #:abs #:min #:max #:sqrt #:random
   #:= #:/= #:< #:<= #:> #:>=
   #:read #:print #:prin1 #:princ #:terpri #:eval))

(in-package #:ulisp)

;; alternatively: https://github.com/anarchodin/ulisp-cl/blob/master/impl.lisp

;; Plan for the port
;; - remove all dependencies
;;   - use controller encryption
;;   - abstract away mailboxes
;;   - remove multithreading
;;     - how to do RX then?
;; - remove utf8 code (emoji)
;; - make `pull' work somehow
;; - make LDB BYTE etc for ulisp
;;
;; Threading:
;; RECEIVE-IN-THREAD => wait on H4-RX, put in RXQ
;; SEND-IN-THREAD => wait on TX mailbox, blocking-send on HW
;; MONITOR-THREAD => wait for stop signal -> kills all threads
;; MAIN => send to TXQ, receive from RXQ
;;
;; Queues:
;; TXQ
;; RXQ (WAIT-NEXT-HCI-RX)
;;
;; App is single-threaded
;; What does a host have to do?
;; - send any outstanding messages
;; - respond to any received messages
;;   -> execute closure on matching procedure reply (eg ATT RSP)
;;   -> see what HTTP lib does for this: blocking or not?
;; - accept messages/commands from the APP
;;
;; => SELECT:
;; - TX outbox
;; - RX inbox (filled by packet reassembler)
;; - APP inbox (isn't this TX also?)
;;
;; what is the difference between APP command/data and ATT discovery RSP
;; ATT discovery rsp doesn't need a UI round-trip
;; ATT RSP needs a UI callback
;;
;; On PC:
;; - H4 RX thread reassembles packets, submits to RXQ
;; - H4 TX thread receives from TXQ, assembles packets & sends synch over H4
;; - Main thread:
;;   - app has to periodically call HOST-PROCESS
;;   HOST-PROCESS:
;;   - put anything from app to TXQ (if has ncp)
;;   - process packets in RXQ
;;     - calls-back app if RSP to APP stimuli
;;   - exit back to APP code
;;   WAIT-EVENT [timeout]
;;   - waits until [timeout] or some other thread wakes up
;;     - have to make WAKEUP function



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

(defparameter *test* '(#x4 #x1 #x3 #xc #x0))

;; Functions I need is:
;; - pull int of arbitrary size + endianness (u8, u16, etc)
;; - advance read pointer of buffer
;; - chain PULL commands together
;;
;; -> need viewmachine again
;;   - ptr: beginning, end, read
;; -> viewmachine can be C++ object w/ data on heap
;;    OR lisp array for prototyping
;; -> PULL returns VIEWM object
;; -> PULL-INT returns an u32 int

(defstruct mview begin end read buffer)

(defun make-memory-view (array)
  (make-mview :begin 0 :end (length array) :read 0 :buffer array))

(defun make-bytes (array)
  ;; array here is #(1 2 3 4)
  (make-array (length array)
              :element-type '(unsigned-byte 8)
              :initial-contents array))


(defparameter *array* (make-bytes #(1 2 3 4 5 6 7 8 9)))
(defun make-memory-view (array)
  "Implementation of MVIEW for CL"
  ;; here ARRAY is a CL array type
  ;; in c++ it would be an opaque type
  (let ((start 0)
        (end (length array))
        (read 0)
        (data array))
    (lambda (msg &optional val)
      (case msg
        (read read)
        (end end)
        ;; consing is expensive in ulisp
        (view (subseq data
                      read (+ read val)))
        (pull (setf read (+ read val)))
        (pull-u8
         (let ((v (aref data read)))
           (setf read (+ read 1))
           v))
        (start start)))))

;; in ulisp, we want to:
;; - return u8, u16 as atomics
;; - convert others to bytes only when necessary

(defun pull-from (mv amount)
  (let ((data (funcall mv 'view amount)))
    (funcall mv 'pull amount)
    data))

(defparameter *test* (make-memory-view *array*))
(pull-from *test* 3)
 ; => #(1 2 3)
(pull-from *test* 3)
 ; => #(4 5 6)
