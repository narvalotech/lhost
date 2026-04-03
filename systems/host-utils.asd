(defsystem "host-utils"
  :description "Utilities for UI"
  :version "0.0.1"
  :author "Jonathan Rico <jonathan@rico.live>"
  :depends-on ("host" "jsonrpc" "usocket" "yason")
  :pathname "../ui"
  :components ((:file "utils")))
