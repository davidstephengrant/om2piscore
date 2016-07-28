(in-package cl-who)

(defparameter *svg* *standard-output*)

(defmacro with-svg (&body body)
  `(progn
     (format *svg* "<?xml version=\"1.0\" standalone=\"yes\"?>~%~%")
     (who:with-html-output
	 (*svg* nil :prologue
		"<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
		:indent T)
       ,@body)))
