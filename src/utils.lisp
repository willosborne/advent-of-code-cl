(defpackage :utils
  (:use :cl)
  (:export #:for-adjacents))
(in-package :utils)

(defparameter adjacencies
  (list '(-1 -1)
        '(0 -1)
        '(1  -1)
        '(-1  0)
        '(0 0)
        '(1  0)
        '(-1  1)
        '(0  1)
        '(1  1)))

(defun for-adjacents (grid x y func &key (diagonals t) (centre nil))
  (let ((max-x (array-dimension grid 1))
        (max-y (array-dimension grid 0)))
    (loop for (dx dy) in adjacencies
          for gx = (min (1- max-x)
                        (max 0 (+ x dx)))
          for gy = (min (1- max-y)
                        (max 0 (+ y dy)))
          do
             (unless (or (and (not centre) (= 0 dx) (= 0 dy))
                         (and (not diagonals) (= (abs dx) (abs dy)) (> (abs dx) 0)))
               (funcall func gx gy (aref grid gy gx)))
          )))


(defun lists-equal-any-order (l1 l2)
  (null (set-exclusive-or l1 l2)))


;; (defvar test-grid (make-array '(4 4) :initial-contents '((1 2 3 4)
;;                                                          (5 6 7 8)
;;                                                          (4 3 2 1)
;;                                                          (8 7 6 5))))

;; (for-adjacents test-grid 1 1
;;                (lambda (x y cell)
;;                  (format t "(~a, ~a): ~a~%" x y cell)) :diagonals nil :centre t)

;; (defun digit-adjacent? (grid x y)
;;   (for-adjacents grid x y
;;                  (lambda (gx gy cell)
;;                    (if (= 5 cell)
;;                        (return-from digit-adjacent? t)))))
;; (defmacro for-adjacents (&key (include-centre nil))) ;
