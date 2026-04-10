(defsystem "host"
  :description "Bluetooth LE Host"
  :version "0.0.1"
  :author "Jonathan Rico <jonathan@rico.live>"
  :depends-on ("local-time" "bordeaux-threads" "sb-concurrency" "sb-posix" "cffi-grovel" "cserial-port" "ironclad")
  :pathname "../"
  :components ((:file "host")))
