(in-package #:ps)

(export '(=>
	  es-class
	  export-from
	  import-from))

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

