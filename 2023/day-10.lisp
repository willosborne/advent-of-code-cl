(defpackage "aoc/2023/10"
  (:use :cl :aoc :iterate :parachute)
  (:import-from :serapeum dict)
  (:import-from :access access)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep if-let))
(in-package "aoc/2023/10")

(define-test day-10)

(defvar *input*)

(defun pull-input ()
  (setf *input* (get-input 2023 10)))

;; copied from day 3
(defun setup-grid (input)
  (let* ((lines (str:lines input))
         (rows (length lines))
         (cols (length (nth 0 lines)))
         (grid (make-array (list rows cols))))
    (iter (for y from 0 below rows)
      (iter (for x from 0 below cols)
        (for cell = (char (nth y lines) x))
        (setf (aref grid y x) cell))
      (finally (return grid)))))

(defun grid-get (grid x y)
  (aref grid y x))

(defun grid-h (grid)
  (array-dimension grid 0))

(defun grid-w (grid)
  (array-dimension grid 1))

(defun grid-set (grid x y val)
  (setf (aref grid y x) val))

(defvar *example-input* (format nil "~
..F7.
.FJ|.
SJ.L7
|F--J
LJ..."))

(defvar *example-grid* (setup-grid *example-input*))

(defparameter compat-list-out
  (dict
   #\| (list :n :s)
   #\- (list :e :w)
   #\. nil
   #\J (list :n :w)
   #\L (list :n :e)
   #\7 (list :w :s)
   #\F (list :s :e)
   #\S (list :n :e :s :w)))

(defun backwards-direction (dir)
  (ecase dir
    (:n :s)
    (:s :n)
    (:e :w)
    (:w :e)))

(defparameter compat-list-in
  (iter (for (k v) in-hashtable compat-list-out)
    (with table = (make-hash-table :test 'equal))
    (setf (gethash k table) (mapcar #'backwards-direction v))
    (finally (return table))))

(defun types-compatible (c1 c2 direction)
  (let ((compat-directions-1 (gethash c1 compat-list-out))
        (compat-directions-2 (gethash c2 compat-list-in)))
    (and (find direction compat-directions-1)
         (find direction compat-directions-2))))

(define-test "checks directions"
  :parent day-10
  (true (types-compatible #\| #\| :n))
  (true (types-compatible #\| #\F :n))

  (false (types-compatible #\| #\- :n))
  (false (types-compatible #\| #\. :n)))

(defun get-direction (dx dy)
  (cond ((= dx -1) :w)
        ((= dx 1) :e)
        ((= dy -1) :n)
        ((= dy 1) :s)
        (t (error "what"))))

(defun cells-compatible? (grid x1 y1 x2 y2)
  (when (or (> (abs (- x1 x2)) 1)
            (> (abs (- y1 y2)) 1)
            (and (= x1 x2)
                 (= y1 y2)))
    (error "bad distances"))
  (let ((c1 (grid-get grid x1 y1))
        (c2 (grid-get grid x2 y2))
        (direction (get-direction (- x2 x1) (- y2 y1))))
    (types-compatible c1 c2 direction)))

(defun find-start (grid)
  (iter (for y from 0 below (array-dimension grid 1))
    (iter (for x from 0 below (array-dimension grid 0))
      (when (char= #\S (aref grid y x))
          (return-from find-start (list x y))))))

(define-test "finds start"
  :parent day-10
  (is equalp '(0 2) (find-start *example-grid*)))


(defun make-new-grid (grid &key initial)
  (make-array (list (grid-h grid) (grid-w grid)) :initial-element initial))

(defun smallest-distance (unvisited distances)
  (iter (for (x . y) in unvisited)
    (finding (cons x y) minimizing (grid-get distances x y))))

(defun all-cells (grid)
  (iter
    (with out = (list))
    (for x from 0 below (grid-w grid))
    (iter (for y from 0 below (grid-h grid))
      (push (cons x y) out))
    (finally (return out))))

(defun dijkstra-weights (grid start-coords)
  (let* ((start-x (car start-coords))
         (start-y (cadr start-coords))
         (unvisited (all-cells grid))

         (distances (make-new-grid grid :initial most-positive-fixnum))
         (previous (make-new-grid grid :initial nil))
         )
    (grid-set distances start-x start-y 0)
    (iter (while unvisited)
      (for (current-x . current-y) = (smallest-distance unvisited distances))
      (setf unvisited (remove (cons current-x current-y) unvisited :test #'equal))

      (format t "~a: ~a ~%" (cons current-x current-y) (grid-get grid current-x current-y))

      (utils:for-adjacents
       grid current-x current-y
       (lambda (gx gy value)
         (when (and (find (cons gx gy) unvisited :test #'equal)
                    (cells-compatible? grid current-x current-y gx gy))
           (format t "  Looking at ~a: ~a ~%" (cons gx gy) value)
           (let ((new-dist (1+ (grid-get distances current-x current-y))))
             (when (< new-dist (grid-get distances gx gy))
               (format t "    Setting ~a to new distance: ~a ~%" (cons gx gy) new-dist)
               (grid-set distances gx gy new-dist)
               (grid-set previous gx gy (cons current-x current-y))))))
       :diagonals nil)
      (finally (return distances)))))


(defun max-dist (distances)
  (iter
    (with maximum = 0)
    (with mx = 0)
    (with my = 0)
    (for x from 0 below (grid-w distances))
    (iter (for y from 0 below (grid-h distances))
      (let ((dist (grid-get distances x y)))

        (when (and (/= dist most-positive-fixnum)
                   (> dist maximum))
          (setf maximum dist)
          (setf mx x)
          (setf my y))))
    (finally (return maximum)))

  ;; (iter (for x from 0 below (grid-w distances))
  ;;   (maximizing
  ;;    (iter (for y from 0 below (grid-h distances))
  ;;      (let ((dist (grid-get distances x y)))

  ;;        (maximizing (grid-get distances x y))))))
  )

(defun solution-1 (&optional (input *input*))
  (let* ((grid (setup-grid input))
         (start (find-start grid))
         (distances (dijkstra-weights grid start)))
    (print distances)
    (max-dist distances)))
