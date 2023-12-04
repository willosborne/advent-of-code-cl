(defpackage "aoc-2022/day-1"
  (:use :cl :aoc/core :iterate))
(in-package "aoc-2022/day-1")

(defvar *input* (get-input 2022 1))

(defun get-fuels ()
  (iter
   (with current-fuel = 0)
   (with out = (list))
   (for line in (str:lines *input*))
   (if (string= "" line)
       (progn (push current-fuel out)
              (setf current-fuel 0))
       (incf current-fuel (parse-integer line)))
    (finally (return out))))

(defun max-fuel ()
  (iter (for fuel in (get-fuels))
    (finding fuel maximizing fuel)))

(defun top-three-sum ()
  (iter (for i in (subseq (sort (get-fuels) #'>) 0 3))
    (summing i)))
