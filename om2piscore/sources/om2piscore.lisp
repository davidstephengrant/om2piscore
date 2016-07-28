;;; Convert from OM to PiScore
(in-package :om2piscore)

(om::defmethod! convert-om2piscore ((n number))
  :initvals '(5)
  :indoc '("<n>")
  :icon 314
  :doc "factorial from a number <n>"
  (if (<= n 1) 1
      (* n (convert-om2piscore (- n 1)))))

;;;USAGE
;;(with-open-file (*svg* "out.svg" :direction :output
;;		       :if-exists :supersede
;;		       :if-does-not-exist :create)
;;  (with-svg 
;;    (:svg :width "4in" :height "3in" :version "1.1"
;;	  :xmlns "http://www.w3.org/2000/svg" :|xmlns:xlink| "http://www.w3.org/1999/xlink"
;;	  (:desc "This graphic links to an external image")
;;
;;	  (:image :x "200" :y "200" :width "100px" :height "100px"
;;		  :|xlink:href| "myimage.png"
;;		  (:title "My image")))))
