(defpackage #:ucg
  (:use #:cl)
  (:export #:make-printer))

(in-package #:ucg)

(defmacro make-printer (name &key terminal default)
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
               (if (gethash (car structure) ,built-in-funcs)
                   (apply (gethash (car structure) ,built-in-funcs)
                          (cdr structure))
                 (apply ,default structure))
             (funcall ,terminal structure)))))))

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
   (lambda (&rest body)
     (concatenate 'string
		  "<" (princ-to-string func-name) ">"
		  (apply #'concatenate 'string
			 (mapcar #'html-grammar body))
		  "</" (princ-to-string func-name) ">"))))

;; and here we print some s-exp using `<name>' function

(html-grammar `(:a (:b (:c "some text " ,(list :a (* 123 321))))))

;; End of example