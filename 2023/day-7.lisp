(defpackage "aoc/2023/7"
  (:use :cl :aoc :iterate :parachute)
  (:import-from :serapeum dict)
  (:import-from :access access)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep if-let))
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
  (> (card-char-to-number v1) (card-char-to-number v2)))

(define-test compare-char
  :parent day-7
  (true (card-value-greater? #\T #\9))
  (false (card-value-greater? #\Q #\A)))

(defvar *rank-order*
  (dict
   :five-of-a-kind 7
   :four-of-a-kind 6
   :full-house 5
   :three-of-a-kind 4
   :two-pair 3
   :one-pair 2
   :high-card 1
   ))

(defun hand-num (hand hand-type-fun)
  (access *rank-order* (funcall hand-type-fun hand)))

(defun compare-hands (hand1 hand2 &optional
                                    (hand-type-fun #'hand-type)
                                    (card-char-num-fun #'card-char-to-number))
  (declare (optimize (debug 3)))
  (let ((r1 (hand-num hand1 hand-type-fun))
        (r2 (hand-num hand2 hand-type-fun)))
    (if (/= r1 r2)
        (> r1 r2)
        (iter
          (for i from 0 below (length hand1))
          (for v1 = (funcall card-char-num-fun (char hand1 i)))
          (for v2 = (funcall card-char-num-fun (char hand2 i)))
          (if (/= v1 v2)
              (return (> v1 v2)))
          (finally
           (error "failed"))))))

(define-test "compares hands"
  :parent day-7
  (false (compare-hands "T55J5" "QQQJA"))
  (true (compare-hands "KK677" "KTJJT"))
  (true (compare-hands "KJ77A" "JQT77")))

(defun parse-input (input)
  (let* ((lines (str:lines input)))
    (iter (for line in lines)
      (for (hand bet) = (str:split " " line))
      (collect (list hand (parse-integer bet))))))

(defun solution-1 (&optional (input *input*))
  (let* ((hands (parse-input input))
         (sorted (sort hands (lambda (h1 h2)
                               (not (compare-hands (car h1) (car h2)))))))
    (iter
      (for rank from 1 to (length sorted))
      (for hand-spec in sorted)
      (summing (* rank (cadr hand-spec))))))


;; part 2

(defun card-char-to-number-2 (ch)
  (or (digit-char-p ch)
      (ccase ch
        (#\T 10)
        (#\J 1)
        (#\Q 12)
        (#\K 13)
        (#\A 14))))

(defun transfer-jokers (types-map)
  (if (or (not (access types-map #\J)) ;; no jokers
          (= 1 (length (alexandria:hash-table-keys types-map)))) ;; only jokers
      types-map
      ;; we have jokers, but not just jokers
      (let ((jokers (access types-map #\J)))
        (remhash #\J types-map)
        (iter (for (k v) in-hashtable types-map)
          (with max-k = nil)
          (with max = 0)
          (if (> v max)
              (setf max-k k
                    max v))
          (finally
           (incf (gethash max-k types-map) jokers)
           (return types-map))) )))

(define-test "transfer jokers"
  :parent day-7
  (is equalp (dict #\K 5) (transfer-jokers (dict #\J 2 #\K 3)))
  (is equalp (dict #\K 4 #\Q 1) (transfer-jokers (dict #\J 1 #\K 3 #\Q 1)))

  )


(defun hand-type-2 (hand)
  (iter (with out = (make-hash-table))
    (for ch in-string hand)
    (if (gethash ch out)
        (incf (gethash ch out))
        (setf (gethash ch out) 1))
    (finally
     (let* ((adjusted-suits (transfer-jokers out))
            (counts (sort (alexandria:hash-table-values adjusted-suits) #'>)))
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

(define-test "parses hand type with jokers"
  :parent day-7
  (is eql :five-of-a-kind (hand-type-2 "KKKKJ"))
  (is eql :four-of-a-kind (hand-type-2 "1JQQQ"))
  (is eql :full-house (hand-type-2 "TT5J5"))
  (is eql :three-of-a-kind (hand-type-2 "KQJJT"))
  (is eql :one-pair (hand-type-2 "KJ2Q1")) ;; note - joker only applies to one!
  (is eql :one-pair (hand-type-2 "AQQ23"))
  (is eql :high-card (hand-type-2 "23456"))
  (is eql :five-of-a-kind (hand-type-2 "JJJJJ")))

(defun solution-2 (&optional (input *input*))
  (let* ((hands (parse-input input))
         (sorted (sort hands (lambda (h1 h2)
                               (not (compare-hands (car h1) (car h2)
                                                   #'hand-type-2 #'card-char-to-number-2))))))
    (print sorted)
    (iter
      (for hand-spec in sorted)
      (for rank upfrom 1)
      (print hand-spec)
      ;; (print rank)
      ;; (print (* rank (cadr hand-spec)))
      (summing (* rank (cadr hand-spec))))))

(defvar *sample-input*
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(define-test "solution 2"
  :parent day-7
  (is = 5905 (solution-2 *sample-input*)))
