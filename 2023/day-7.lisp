(defpackage "aoc/2023/7"
  (:use :cl :aoc :iterate :parachute)
  (:import-from :serapeum dict)
  (:import-from :access access)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep))
(in-package "aoc/2023/7")

(define-test day-7)

(defvar *input* (get-input 2023 7))

(defun hand-type (hand)
  (iter (with out = (make-hash-table))
    (for ch in-string hand)
    (if (gethash ch out)
        (incf (gethash ch out))
        (setf (gethash ch out) 1))
    (finally
     (let ((counts (sort (alexandria:hash-table-values out) #'>)))
       (return
         (cond ((= (car counts) 5) :five-of-a-kind)
               ((= (car counts) 4) :four-of-a-kind)
               ((and (= (car counts) 3)
                     (= (length counts) 2)) :full-house)
               ((and (= (car counts) 3)
                     (/= (length counts) 2)) :three-of-a-kind)
               ((and (= (car counts) 2)
                     (= (cadr counts) 2)) :two-pair)
               ((and (= (car counts) 2)
                     (= (length counts) 4)) :one-pair)
               (t :high-card)

               ))))))

(define-test "parses hand type"
  :parent day-7
  (is eql :five-of-a-kind (hand-type "AAAAA"))
  (is eql :four-of-a-kind (hand-type "AA8AA"))
  (is eql :full-house (hand-type "23332"))
  (is eql :three-of-a-kind (hand-type "TTT98"))
  (is eql :two-pair (hand-type "23432"))
  (is eql :one-pair (hand-type "A23A4"))
  (is eql :high-card (hand-type "23456")))

(defun card-char-to-number (ch)
  (or (digit-char-p ch)
      (ccase ch
        (#\T 10)
        (#\J 11)
        (#\Q 12)
        (#\K 13)
        (#\A 14))))

(define-test parse-chars
  :parent day-7
  (is = 7 (card-char-to-number #\7))
  (is = 10 (card-char-to-number #\T))
  (is = 11 (card-char-to-number #\J))
  (is = 12 (card-char-to-number #\Q))
  (is = 13 (card-char-to-number #\K))
  (is = 14 (card-char-to-number #\A))

  )

(defun card-value-greater? (v1 v2)
  )
