(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "parenscript"))

(asdf:defsystem #:pjs-parenscript-next
     :depends-on ("parenscript")
     :serial t
     :components ((:file "es-next")
		  (:file "import")
		  (:file "export")))
