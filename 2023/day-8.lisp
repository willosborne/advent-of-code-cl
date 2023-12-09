(defpackage "aoc/2023/8"
  (:use :cl :aoc :iterate :parachute)
  (:import-from :serapeum dict)
  (:import-from :access access)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep if-let))
(in-package "aoc/2023/8")

(define-test day-8)

(defvar *input*)

(defun pull-input ()
  (setf *input* (get-input 2023 8)))

(defun parse-rule (line)
  (cl-ppcre:all-matches-as-strings "[A-Z]+" line))

(define-test "parses line"
  :parent day-8
  (is equalp '("AAA" "BBB" "CCC") (parse-rule "AAA = (BBB, CCC)")))

(defun parse-input (input)
  (let* ((lines (str:lines input))
         (choices (car lines))
         (rules (mapcar #'parse-rule (cddr lines))))
    (values choices rules)))

(defun follow-rule (rule lr)
  (if (char= lr #\L)
      (second rule)
      (third rule)))

(define-test "follows rule"
  :parent day-8
  (is string= "BBB" (follow-rule '("AAA" "BBB" "CCC") #\L))
  (is string= "CCC" (follow-rule '("AAA" "BBB" "CCC") #\R)))

(defun apply-rules (rules input lr)
  (let ((rule (find-if (lambda (r) (string= (car r) input))
                       rules)))
    (follow-rule rule lr)))

(define-test "follows rules"
  :parent day-8
  (is string= "BBB" (apply-rules '(("AAA" "BBB" "CCC")
                                   ("DDD" "EEE" "FFF")) "AAA" #\L))
  (is string= "EEE" (apply-rules '(("AAA" "BBB" "CCC")
                                   ("DDD" "EEE" "FFF")) "DDD" #\L))
  )

(defun run-for-input (choices rules start end-p)
  (iter
    (with state = start)
    (for i upfrom 0)
    (for lr = (char choices (mod i (length choices))))
    (setf state (apply-rules rules state lr))
    ;; (format t "~a ~a~%" i state)
    (when (funcall end-p state)
      (print state)
      (return (1+ i)))))

(defun solution-1 (&optional (input *input*))
  (multiple-value-bind (choices rules) (parse-input input)
    (run-for-input choices rules "AAA" (lambda (state)
                                         (string= state "ZZZ")))))


;; part 2
;;

(defun index-rules (rules-list)
  (iter (for rule in rules-list)
    (with out = (make-hash-table :test #'equal))
    (setf (gethash (car rule) out) rule)
    (finally (return out))))

(defun apply-rules-indexed (rules-map input lr)
  (let ((rule (gethash input rules-map)))
    (follow-rule rule lr)))

(defun run-for-input-simulaneous (choices rules-map starts end-p)
  (print starts)
  (iter
    (with states = starts)
    (for i upfrom 0)
    (for lr = (char choices (mod i (length choices))))
    (setf states
          (iter (for state in states)
            (collect (apply-rules-indexed rules-map state lr))))
    (if (= 0 (mod i 100000))
        (format t "~a ~a~%" i states))
    (when (every end-p states)
      (print states)
      (return (1+ i)))))

(defun get-starts (rules)
  (iter (for rule in rules)
    (when (str:ends-with? #\A (car rule))
      (collect (car rule))))
  )



(defun solution-2 (&optional (input *input*))
  (multiple-value-bind (choices rules) (parse-input input)
    (iter (for start in (get-starts rules))
      (collect (run-for-input choices rules start (lambda (state)
                                                    (str:ends-with? #\Z state)))
        into nums)
      (finally (return (apply #'lcm nums)))
      )
    ))


;; massive amounts of crap experimenting
;; i don't understand why this stuff isn't needed, why are the starts not affecting the overall LCM

(defun solution-2-test (&optional (input *input*))
  (multiple-value-bind (choices rules) (parse-input input)
    (run-for-input-simulaneous choices
                               (index-rules rules)
                               (get-starts rules)
                               (lambda (state)
                                 (str:ends-with? #\Z state)))))


(defun find-start-and-period (choices rules-map start)
  (iter
    (with state = start)
    (with visited = (make-hash-table :test 'equal))
    (for i upfrom 0)
    (for lr = (char choices (mod i (length choices))))
    (for next = (apply-rules-indexed rules-map state lr))
    (setf (gethash state visited) i)

    (when (str:ends-with? #\Z state)
      (let ((end-idx i)
            (start-idx (gethash next visited)))
        (return (list start-idx (- end-idx start-idx))))
      )

    (setf state next)
    ))

(defun compute-end (lcm total-starts n)
  (iter (for i upfrom 1)
    (for nx = (+ (* i n lcm) total-starts))
    ;; (format t "nx: ~a i: ~a nx/n: ~a ~%" nx i (mod nx n))
    (if (= 0 (mod nx n))
        (return (/ nx n))))
  )

(defun find-end-point (choices rules-map starts)
  (iter (for start in starts)
    (for s-p = (find-start-and-period choices rules-map start))
    (collect (cadr s-p) into periods)
    (collect (car s-p) into start-idxs)
    (print periods)
    (finally (let* ((lcm (apply #'lcm periods))
                    (all-starts (reduce #'+ start-idxs)))
               (print lcm)
               (return (compute-end lcm all-starts (length starts)))
               )
             )
  )
)
