(in-package #:parenscript)

(defvar *import-as-require* nil)

(defun sort-name-forms (name-forms)
  (sort name-forms
	(lambda (a b)
	  (if (symbolp a)
	      (if (symbolp b)
		  (string< a b)
		  ;; else
		  t)
	      ;; else
	      (if (symbolp b)
		  nil
		  ;; else
		  nil)))))

(define-statement-operator import-from (module &body name-forms)
  (if *import-as-require*
      (compile-statement (let ((req `(require ,module))
			       (name-forms (sort-name-forms name-forms)))
			   (if (null name-forms)
			       req
			       ;; else
			       (let (module-name)
				 `(progn
				    ,@ (mapcar (lambda (form)
						 (if (symbolp form)
						     (progn
						       (setf module-name form)
						       `(var ,form ,req))
						     ;; else
						     (if (null (rest form))
							 `(var ,(first form)
							    (@ ,(or module-name
								    req)
							       ,(first form)))
							 ;; else
							 (if module-name
							     `(progn
								,@ (mapcar (lambda (name)
									     `(var ,name
										(@ ,module-name
										   ,name)))
									   form))
							     ;; else
							     (let ((sym (gensym "MODULE-")))
							       `(progn
								  ,@ (mapcar (lambda (name)
									       `(var ,name))
									     form)
								     (let ((,sym ,req))
								       ,@ (mapcar (lambda (name)
										    `(setf ,name (@ ,sym ,name)))
										  form)
								       nil)))))))
					       name-forms))))))
      ;; else
      `(import-from ,module ,name-forms)))

(defprinter import-from (module name-forms)
	    (psw "import ")
	    (when name-forms
	      (labels ((delimited (forms start end)
			 (psw start)
			 (comma-separated forms)
			 (psw end))
		       (comma-separated (forms)
			 (loop #:while forms
			       #:do
				 (name (pop forms))
				 (when forms
				   (psw ", "))))
		       (name (form)
			 (cond
			   ((eq form '*)
			    (psw "*"))
			   ((symbolp form)
			    (ps-print form))
			   ((and (listp form)
				 (eq (first form)
				     'array))
			    (delimited (rest form)
				       "[ "
				       " ]"))
			   ((listp form)
			    (delimited form
				       "{ "
				       " }"))
			   (t
			    (error "unhandled form ~w" form)))))
		(comma-separated name-forms))
	      (psw " from "))
	    (psw "'")
	    (psw module)
	    (psw "'"))

(defmacro import-from (module &body forms)
  (declare (ignore module forms)))
