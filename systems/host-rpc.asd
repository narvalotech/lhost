(defsystem "host-rpc"
  :description "JRPC BLE server"
  :version "0.0.1"
  :author "Jonathan Rico <jonathan@rico.live>"
  :depends-on ("host-utils" "jsonrpc" "usocket" "yason")
  :pathname "../ui"
  :components ((:file "jsonrpc")))
