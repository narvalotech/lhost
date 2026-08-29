(asdf:defsystem "prototest"
  :description "Testing PB on BLE"
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on (:cl-protobufs)
  :components
  ((:protobuf-source-file "prototest"
    :proto-pathname "prototest.proto")))
