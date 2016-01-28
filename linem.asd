(in-package #:cl-user)
(asdf:defsystem linem
  :description "Cute Lines!"
  :author "resttime"
  :version "1.0"
  :serial t
  :components ((:file "package")
	       (:file "canvas")
	       (:file "linem"))
  :depends-on (:qtools :qtcore :qtgui))
