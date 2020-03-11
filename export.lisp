(in-package #:parenscript)

(define-statement-operator export (form &optional form2)
  (if form2
      (progn
	(assert (eq form :default))
	`(export t ,(compile-statement form2)))
      ;;
      `(export nil ,(compile-statement form))))

(defprinter export (default form)
	    (psw "export ")
	    (when default
	      (psw "default "))
	    (ps-print form))

(define-statement-operator export-from (module &body forms)
  `(export-from ,module ,forms))

(defprinter export-from (module forms)
	    (let (seen)
	      (psw "export { ")
	      (dolist (form forms)
		(if seen
		    (psw ", ")
		    ;;
		    (setf seen t))
		(if (listp form)
		    (destructuring-bind (as name init)
			form
		      (assert (eq :as as))
		      (ps-print init)
		      (psw " as ")
		      (ps-print name))
		    ;;
		    (ps-print form)))
	      (psw " } from ")
	      (ps-print module)))

(ps (export-from "use-data-fetch"
		 use-data-fetch))
