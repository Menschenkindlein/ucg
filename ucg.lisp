(defpackage #:ucg
  (:use #:cl)
  (:export #:make-printer))

(in-package #:ucg)

(defmacro make-eval (name &key terminal default)
  (let ((funcs-and-macros (gensym)))
    `(let ((,funcs-and-macros (make-hash-table)))
       (defun ,(intern (format nil "~a-DEFUN" name))
           (name lambda)
         (setf (gethash name ,funcs-and-macros)
               (cons :function lambda)))
       (defun ,(intern (format nil "~a-DEFMACRO" name))
           (name lambda)
         (setf (gethash name ,funcs-and-macros)
               (cons :macro lambda)))
       (defun ,name (structure)
         (when structure
           (if (listp structure)
               (if (gethash (car structure) ,funcs-and-macros)
		   (case (car (gethash (car structure) ,funcs-and-macros))
		     (:function
		      (apply (cdr (gethash (car structure) ,funcs-and-macros))
			     (mapcar #',name (cdr structure))))
		     (:macro
		      (,name
		       (apply (cdr (gethash (car structure) ,funcs-and-macros))
			      (cdr structure)))))
		   (apply ,default structure))
	       (funcall ,terminal structure)))))))

;; Example eval

;; here we define eval and basic macros

(make-eval html-grammar
	   :terminal (lambda (terminal) (princ-to-string terminal))
	   :default (lambda (&rest unknown)
		      (apply #'concatenate
			     'string
			     (mapcar #'html-grammar
				     unknown))))

;; and here we add some specific functions, using `add-<name>-structure'

(dolist (func-name (list :a :b :c))
  (html-grammar-defun func-name
   (lambda (&rest body)
     (concatenate 'string
		  "<" (princ-to-string func-name) ">"
		  (apply #'concatenate 'string
			 (mapcar #'html-grammar body))
		  "</" (princ-to-string func-name) ">"))))

(html-grammar-defmacro 'list
		       (lambda (&rest args)
			 args))

;; and here we eval some s-exp using `<name>' function

(html-grammar `(:a (:b (:c "some text " (list :a ,(* 123 321))))))

;; End of example