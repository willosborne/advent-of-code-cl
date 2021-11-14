(defpackage aoc-2019-3
  (:use :cl :arrow-macros)
  (:import-from :1am :run :test :is)
  (:import-from :lol :nlet))
(in-package :aoc-2019-3)

(defstruct vec x y)

(defun vec (x y)
  (make-vec :x x :y y))

(defstruct line p1 p2)

(defun line (x1 y1 x2 y2)
  (make-line :p1 (vec x1 y1)
             :p2 (vec x2 y2)))

(defun line-delta (line)
  (vec- (line-p2 line) (line-p1 line)))

(defun line-length (line)
  (let ((delta (line-delta line)))
    (+ (abs (vec-x delta))
       (abs (vec-y delta)))))

(defun vec+ (v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

(defun vec- (v1 v2)
  (vec (- (vec-x v1) (vec-x v2))
       (- (vec-y v1) (vec-y v2))))

(defun vec-cross (v1 v2)
  (- (* (vec-x v1) (vec-y v2))
     (* (vec-x v2) (vec-y v1))))

(defun vec-dot (v1 v2)
  (+ (* (vec-x v1) (vec-x v2))
     (* (vec-y v1) (vec-y v2))))

(defun vec* (v s)
  (vec (* (vec-x v) s)
       (* (vec-y v) s)))


(defun scalar-meeting (start delta target)
  (let* ((t-meeting (/ (- target start)
               delta)))
    (when (and (> t-meeting 0) (< t-meeting 1))
      (+ start (* t-meeting delta)))))

(defun line-intersection (l1 l2)
  (when (zerop (vec-dot (line-delta l1) (line-delta l2)))
    (let* ((vertical-line (if (zerop (vec-x (line-delta l1))) l1 l2))
           (horizontal-line (if (zerop (vec-x (line-delta l1))) l2 l1))
           (x (scalar-meeting (vec-x (line-p1 horizontal-line))
                              (vec-x (line-delta horizontal-line))
                              (vec-x (line-p1 vertical-line))))
           (y (scalar-meeting (vec-y (line-p1 vertical-line))
                              (vec-y (line-delta vertical-line))
                              (vec-y (line-p1 horizontal-line)))))
      (when (and x y)
        (vec x y)))))

(test test-line-intersection
  (is (not (line-intersection (line 0 0 0 10) (line 3 5 10 5))))
  (is (not (line-intersection (line 20 0 20 10) (line 3 5 10 5))))
  (is (not (line-intersection (line 0 0 0 10) (line 10 0 10 10))))
  (is (not (line-intersection (line 0 0 10 0) (line 0 10 10 10)))))

(defun manhattan-distance (v)
  (+ (abs (vec-x v))
     (abs (vec-y v))))

;; (defun min-by (func list)
;;   (loop for val in list
;;         with min = (most-positive-fixnum)
;;         do (if ())))

(defun collect-intersections (wire1 wire2)
  (loop for line1 in wire1
        with out = '()
        do (setf out
                 (nconc out
                        (loop for line2 in wire2
                              for intersection = (line-intersection line1 line2)
                              when intersection
                                collect intersection)))
        finally (return out)))

(defun parse-statement (stmt)
  (let* ((direction (str:s-first stmt))
         (dir-sym (alexandria:switch (direction :test 'equal)
                    ("U" :up)
                    ("D" :down)
                    ("L" :left)
                    ("R" :right)))
         (number (parse-integer (str:s-rest stmt))))
    (list :dir dir-sym :mag number)))

(defun get-vec (direction magnitude)
  (ecase direction
    (:up (vec 0 magnitude))
    (:down (vec 0 (- magnitude)))
    (:left (vec (- magnitude) 0))
    (:right (vec magnitude 0))))

(defun build-wire (directions)
  (lol:nlet-tail run ((directions directions)
                      (lines nil)
                      (position (vec 0 0)))
                 (if (consp directions)
                     (let* ((stmt (car directions))
                            (vector (get-vec (getf stmt :dir) (getf stmt :mag)))
                            (next (vec+ position vector))
                            (line (make-line :p1 position :p2 next)))
                       (run (cdr directions)
                            (cons line lines)
                            next))
                     (nreverse lines))))


(defun parse-line (line)
  (let* ((strings (str:split #\, line))
         (statments (mapcar #'parse-statement strings)))
    statments))

(defun get-wires (input)
  ;; (let* ((input (str:lines (aoc:get-input 2019 3)))))
  (->> input
    (str:lines)
    (mapcar #'parse-line)
    (mapcar #'build-wire)))

(defun get-intersections (wires)
  (destructuring-bind (wire1 wire2) wires
    (collect-intersections wire1 wire2)))

(defun get-answer-1 ()
  (->> (aoc:get-input 2019 3)
    (get-wires)
    (get-intersections)
    (mapcar #'manhattan-distance)
    (apply #'min)))

(defvar test-input-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")


(defun collect-intersections-steps (wire1 wire2)
  (loop for line1 in wire1
        with out = '()
        with steps-l1 = 0
        do (incf steps-l1 (line-length line1))
           (setf out
                 (nconc out
                        (loop for line2 in wire2
                              with steps-l2 = 0
                              for intersection = (line-intersection line1 line2)
                              do (incf steps-l2 (line-length line2))
                              when intersection
                                collect (list intersection steps-l1 steps-l2))))
        finally (return out)))

(defun intersections-steps (wire1 wire2)
  (nlet loop-1 ((lines-1 wire1)
                (steps-1 0)
                (intersections '()))
    (if (consp lines-1)
        (let ((outer-line (car lines-1)))
          (loop-1 (cdr lines-1)
                  (+ (line-length outer-line) steps-1)
                  (nconc intersections
                         (nlet loop-2 ((lines-2 wire2)
                                       (steps-2 0)
                                       (intersections '()))
                           (if (consp lines-2)
                               (let* ((inner-line (car lines-2))
                                      (intersection (line-intersection outer-line inner-line)))
                                 (loop-2 (cdr lines-2)
                                         (+ steps-2 (line-length inner-line))
                                         (if intersection
                                             (cons (list intersection
                                                         (+ steps-1
                                                            (manhattan-distance (vec- intersection
                                                                                      (line-p1 outer-line)))
                                                            steps-2
                                                            (manhattan-distance (vec- intersection
                                                                                      (line-p1 inner-line)))))
                                                   intersections)
                                             intersections)))
                               intersections)))))
        intersections)))

(nlet loop2 ((n 0))
  (format t "~a~%" n)
  (if (> n 100)
      n
      (loop2 (1+ n))))


(defun get-intersections-steps (wires)
  (destructuring-bind (wire1 wire2) wires
    (intersections-steps wire1 wire2)))

(defun get-answer-2 ()
  (let ((intersections
          (->> (aoc:get-input 2019 3)
            (get-wires)
            (get-intersections-steps))))
    (loop for (intersection steps) in intersections
          minimizing steps)))
