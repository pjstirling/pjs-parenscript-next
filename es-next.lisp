(in-package #:ps)

(export '(=> es-class import-from))

(define-expression-operator => (lambda-list &rest body)
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body)
    `(=> ,effective-args
       ,(let ((*function-block-names* ()))
          (compile-function-body effective-args effective-body)))))

(define-statement-operator es-class (name (&optional parent) &body body)
  (let (defs)
    (flet ((collect (form)
	     (push form defs))
	   (parse (sym form)
	     (destructuring-bind (name (&rest params) &body body)
		 form
	       (multiple-value-bind (effective-args body-block docstring)
		   (compile-named-function-body name params body)
		 (list sym name effective-args docstring body-block)))))
      (dolist (form body)
	(ecase (first form)
	  (static
	   (dolist (form (rest form))
	     (ecase (first form)
	       (setf
		(pop form)
		(loop #:while form
		      #:do
			(collect `(static-assign ,(pop form)
						 ,(compile-expression (pop form))))))
	       ((defun)
		(collect (parse 'static-function
				(rest form)))))))
	  (setf
	   (pop form)
	   (loop #:while form
		 #:do
		   (collect `(setf ,(pop form)
				   ,(compile-expression (pop form))))))
	  ((defun)
	   (collect (parse 'function
			   (rest form))))))
      `(es-class ,name ,(when parent
			  (compile-expression parent))
	 ,(nreverse defs)))))

(defvar *import-as-require* t)

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

(defun print-fun-args (args)
  (psw "(")
  (loop #:while args
	#:do (ps-print (pop args))
	     (when args
	       (psw ", ")))
  (psw ")"))

(defprinter => (args body)
	    (print-fun-args args)
	    (psw " => ")
	    (ps-print body))

(defmacro => ((&rest args) &body body)
  (declare (ignore args body)))

(defprinter es-class (name parent body)
	       "class "
	       (ps-print name)
	       (when parent
		 (psw " extends ")
		 (ps-print parent))
	       " {"
	       (incf *indent-level*)
	       (labels ((fun (form)
			  (destructuring-bind (name args docstring body)
			      form
			    (declare (ignore docstring))
			    (psw (symbol-to-js-string name))
			    (print-fun-args args)
			    (psw " ")
			    (ps-print body))))
		 (dolist (form body)
		   (newline-and-indent)
		   (ecase (first form)
		     (static-assign
		      (psw "static ")
		      (ps-print (second form))
		      (psw " = ")
		      (ps-print (third form)))
		     (static-function
		      (psw "static ")
		      (fun (rest form)))
		     (setf
		      (ps-print (second form))
		      (psw " = ")
		      (ps-print (third form)))
		     (function
		      (fun (rest form))))))
	       (decf *indent-level*)
	       (newline-and-indent)
	       (psw "}"))

(defmacro es-class (name (&optional parent) &body forms)
  (declare (ignore name parent forms)))

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
				       "["
				       "]"))
			   ((listp form)
			    (delimited form
				       "{"
				       "}"))
			   (t
			    (error "unhandled form ~w" form)))))
		(comma-separated name-forms))
	      (psw " from "))
	    (psw "'")
	    (psw module)
	    (psw "'"))

(defmacro import-from (module &body forms)
  (declare (ignore module forms)))
