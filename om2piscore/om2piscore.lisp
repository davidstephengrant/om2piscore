(defvar om2piscore)

(defpackage om2piscore)

(in-package :om2piscore)

(mapc 'om::compile&load
      (list
       (make-pathname :directory
		      (append
		       (pathname-directory *load-pathname*)
		       (list "sources"))
		      :name "om2piscore"
		      :type "lisp")))

(om::fill-library 
 '(("Conversion" nil nil (om2piscore) nil)))
