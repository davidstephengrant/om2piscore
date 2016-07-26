;;; Convert from OM to PiScore
(in-package :om2piscore)

(om::defmethod! convert-om2piscore ((n number))
  :initvals '(5)
  :indoc '("<n>")
  :icon 314
  :doc "factorial from a number <n>"
  (if (<= n 1) 1
      (* n (convert-om2piscore (- n 1)))))
