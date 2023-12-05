(defpackage "aoc/2023/5"
  (:use :cl :aoc :iterate :parachute :alexandria :access)
  (:import-from :serapeum dict)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep))
(in-package "aoc/2023/5")

(define-test day-5)

(defvar *input* (get-input 2023 5))

(serapeum:toggle-pretty-print-hash-table)

(defparameter *lookup-order* (list :soil
                                   :fertilizer
                                   :water
                                   :light
                                   :temperature
                                   :humidity
                                   :location))


(defun parse-seeds (seed-line)
  (arrows:->> (str:split ": " seed-line)
              (nth 1)
              (str:split " ")
              (mapcar #'parse-integer)))

(define-test "parses seed line"
  :parent day-5
  (is equalp '(123 34 2 1 2) (parse-seeds "seeds: 123 34 2 1 2")))

(defun is-range-line (line)
  (cl-ppcre:scan "\\d" line))

(define-test "identifies range line"
  :parent day-5
  (true (is-range-line "123 3 21 2"))
  (false (is-range-line ""))
  (false (is-range-line "seed-to-soil map:")))

(defun get-lookup-type (lookup-line)
  (cl-ppcre:register-groups-bind (words) ("\\w+-(\\w+) map:" lookup-line)
    (utils:to-keyword words)))

(define-test "extracts lookup type"
  :parent day-5
  (is eql :soil (get-lookup-type "seed-soil map:"))
  (false (get-lookup-type "")))

(defun parse-input (input)
  (let* ((lines (str:lines input))
         (seeds (parse-seeds (car lines)))
         (out (make-hash-table))
         (lookup nil)
         (ranges (list)))
    (iter (for line in (cddr lines))
      (alexandria:if-let ((lookup-type (get-lookup-type line)))
        (setf lookup lookup-type
              ranges (list)))
      (if (is-range-line line)
          (push (mapcar #'parse-integer (str:split " " line)) ranges))
      (if (str:empty? line)
          (setf (gethash lookup out) (nreverse ranges)))
      (finally
       (setf (gethash lookup out) (nreverse ranges))
       (return (dict :seeds seeds :lookups out))))

    )

  )

(defvar *sample-input* (format nil "~
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"))

(define-test "parses whole input"
  :parent day-5
  (let* ((parsed (parse-input *sample-input*))
         (seeds (gethash :seeds parsed))
         (lookups (gethash :lookups parsed)))
    (is equalp '(79 14 55 13) seeds)
    (is equalp '((50 98 2)
                 (52 50 48)) (gethash :soil lookups))
    (is utils:lists-equal-any-order *lookup-order* (hash-table-keys lookups))))

(defparameter *sample-lookups* (parse-input *sample-input*))

(defun range-valid (dest src range number)
  (if (and (>= number src)
           (< number (+ src range)))
      (+ dest (- number src))))

(define-test "check valid ranges"
  :parent day-5
  (is = 50 (range-valid 50 98 2 98))
  (is = 51 (range-valid 50 98 2 99))
  )

(defun run-lookup (number lookups)
  (iter (for (dest src range) in lookups)
    (if-let ((val (range-valid dest src range number)))
      (return val))
    (finally (return number))
    ))

(define-test "looks up one number"
  :parent day-5
  (let ((soil-lookups (access (access *sample-lookups* :lookups) :soil)))
    (is = 50 (run-lookup 98 soil-lookups))
    (is = 51 (run-lookup 99 soil-lookups))
    (is = 55 (run-lookup 53 soil-lookups))
    (is = 9999 (run-lookup 9999 soil-lookups)))
  )

(defun run-all-lookups-1 (number lookups)
  (iter (with val = number)
    ;; (print val)                         ;
    (for lookup-type in *lookup-order*)
    (for lookup = (access lookups lookup-type))
    ;; (print lookup)
    (setf val (run-lookup val lookup))
    (finally (return val))
    )
  )

(define-test "runs all lookups for one num"
  :parent day-5
  (let ((lookups (access *sample-lookups* :lookups)))
    (is = 82 (run-all-lookups-1 79 lookups))))

(defun solution-1 (&optional (input *input*))
  (let* ((parsed (parse-input input))
         (seeds (access parsed :seeds))
         (lookups (access parsed :lookups)))
    (iter (for seed in seeds)
      (minimize (run-all-lookups-1 seed lookups)))))

(define-test "solution 1 runs on sample"
  :parent day-5
  (is = 35 (solution-1 *sample-input*)))

;; part 2

(defun solution-2 (&optional (input *input*))
  (let* ((parsed (parse-input input))
         (seeds (access parsed :seeds))
         (lookups (access parsed :lookups)))
    ;; (iter (for seed in seeds)
    ;;   (minimize (run-all-lookups-1 seed lookups)))
    (iter (for (min range) in (serapeum:batches seeds 2))
      (with n = 0)
      (minimize
       (iter (for i from min below (+ min range))
         (incf n)
         (if (= 0 (mod n 1000000))
             (print n))
         (minimize (run-all-lookups-1 i lookups)))))))
