(defpackage #:ucg
  (:use #:cl))

(in-package #:ucg)

(defgeneric add-structure (grammar
			   structure-name
			   printing-lambda)
  (:documentation "Defines how the rules for printing are added."))

(defgeneric print-in (grammar structure)
  (:documentation "Defines how to apply printing functions to structure."))

(defmacro make-printer (name &key terminal default)
  (let ((grammar (gensym)))
    `(progn (defclass ,grammar nil
	      ((built-in-funcs :initform (make-hash-table)
			       :accessor built-in-funcs)
	       (default-lambda :initarg :default
		               :reader default-lambda)
	       (terminal-lambda :initarg :terminal
				:reader terminal-lambda)))
	    (defmethod add-structure ((,grammar ,grammar)
				      structure-name
				      printing-lambda)
	      (setf (gethash structure-name (built-in-funcs ,grammar))
		    printing-lambda))
	    (defmethod print-in ((,grammar ,grammar) structure)
	      (when structure
		(if (listp structure)
		    (if (gethash (car structure)
				 (built-in-funcs ,grammar))
			(apply (gethash (car structure)
					(built-in-funcs ,grammar))
			       structure)
			(if (fboundp (car structure))
			    (print-in ,grammar
				      (apply (car structure)
					     (cdr structure)))
			    (apply (default-lambda ,grammar) structure)))
		    (funcall (terminal-lambda ,grammar) structure))))
	    (defparameter ,grammar
	      (make-instance ',grammar
			     :terminal ,terminal
			     :default ,default))
	    (defun ,name (structure)
	      (print-in ,grammar structure))
	    (defun ,(intern (concatenate 'string
					 "ADD-"
					 (princ-to-string name)
					 "-STRUCTURE"))
		(structure-name printing-lambda)
	      (add-structure ,grammar
			     structure-name
			     printing-lambda)))))

;; Example printer

;; here we define grammar and basic printing functions

(make-printer html-grammar
	      :terminal (lambda (terminal) (princ-to-string terminal))
	      :default (lambda (&rest unknown)
			 (apply #'concatenate
				'string
				(mapcar #'html-grammar
					unknown))))

;; and here we add one specific function, using `add-<name>-structure'

(dolist (func-name (list :a :b :c))
  (add-html-grammar-structure func-name
   (lambda (func &rest body)
     (concatenate 'string
		  "<" (princ-to-string func) ">"
		  (apply #'concatenate 'string
			 (mapcar #'html-grammar body))
		  "</" (princ-to-string func) ">"))))

;; and here we print some s-exp using `<name>' function

(html-grammar '(:a (:b (:c "some text" (:d test)))))

;; End of example