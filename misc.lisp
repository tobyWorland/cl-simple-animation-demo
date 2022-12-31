
(in-package :animation)

(defun random-in-range (min max)
  "Return a random integer between the inclusive range MIN to MAX."
  (assert (<= min max))
  (+ min (random (- max min -1))))
