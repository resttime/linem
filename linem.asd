(in-package #:cl-user)
(asdf:defsystem linem
  :description "Cute Lines!"
  :serial t
  :components ((:file "package")
	       (:file "canvas")
	       (:file "linem"))
  :depends-on (:qtools :qtcore :qtgui))
