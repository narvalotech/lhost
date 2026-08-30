(push "/home/jon/lhost/scripts/proto" ql:*local-project-directories*)

(ql:quickload :cl-protobufs)
(ql:quickload :prototest)
(ql:quickload :host)

(defpackage #:ptest
  (:use #:common-lisp)
  (:use #:host)
  (:local-nicknames (#:p #:cl-protobufs.prototest)
                    (#:clp #:cl-protobufs)))
(in-package :ptest)

(defun pyhex (bytes)
  "Output in python-compatible hex format"
  (with-output-to-string (s)
    (loop for byte across bytes
          for first = t then nil
          do (unless first (write-char #\Space s))
             (format s "~(~2,'0x~)" byte))))

(defparameter *tlist*
  (list
   (p:make-tee :range 100 :dist-x -10 :dist-y -400)
   (p:make-tee :range 10 :dist-x -10)
   (p:make-tee :range 100 :dist-y -120)))

(defparameter *o1*
  (p:make-omnibus
   :state :state-running
   :flags #x01
   :objects *tlist*))

(pyhex (clp:serialize-to-bytes *o1*))
