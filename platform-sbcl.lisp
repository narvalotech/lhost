(require 'asdf)
(require :bordeaux-threads)
(require :local-time)

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
