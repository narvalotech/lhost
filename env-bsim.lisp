(defparameter *h2c-path*   "/tmp/lhost/uart.h2c")
(defparameter *c2h-path*   "/tmp/lhost/uart.c2h")
(defparameter *fifo-paths*
  (list :h2c *h2c-path*
        :c2h *c2h-path*))

(defvar *packetizer-path* nil)
(setf *packetizer-path* *fifo-paths*)
