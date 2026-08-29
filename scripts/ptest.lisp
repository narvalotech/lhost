(push "/home/jon/lhost/scripts/proto" ql:*local-project-directories*)

(ql:quickload :cl-protobufs)
(ql:quickload :prototest)
(ql:quickload :host)

(defpackage #:ptest
  (:use #:common-lisp)
  (:use #:host)
  (:local-nicknames (#:p #:cl-protobufs.prototest)))
(in-package :ptest)
