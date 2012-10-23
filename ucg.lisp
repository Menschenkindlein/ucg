(defpackage #:ucg
  (:use #:cl))

(in-package #:ucg)

(defgeneric add-structure (grammar
			   structure-name
			   printing-lambda)
  (:documentation "Defines how the rules for printing are added."))

(defgeneric print-in (grammar structure)
  (:documentation "Defines how to apply printing functions to structure."))

(defmacro make-printer (name &key terminal default escape-symbol)
  (let ((built-in-funcs (gensym)))
    `(let ((,built-in-funcs (make-hash-table)))
       (defun ,(intern (concatenate 'string
				    "ADD-"
				    (princ-to-string name)
				    "-STRUCTURE"))
	   (structure-name printing-lambda)
	 (setf (gethash structure-name ,built-in-funcs)
	       printing-lambda))
       (defun ,name (structure)
	 (when structure
	   (if (listp structure)
	       (if (eql ,escape-symbol (car structure))
		   (funcall #',name
			    (eval (second structure)))
		   (if (gethash (car structure) ,built-in-funcs)
			    (apply (gethash (car structure) ,built-in-funcs)
				   structure)
			    (apply ,default structure)))
		    (funcall ,terminal structure)))))))

;; Example printer

;; here we define grammar and basic printing functions

(make-printer html-grammar
	      :escape-symbol 'raw-lisp
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

(html-grammar '(:a (:b (:c "some text" (raw-lisp (list :a " " (* 123 321)))))))

;; End of example