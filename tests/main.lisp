(defpackage aoc/tests/main
  (:use :cl
        :aoc
        :rove))
(in-package :aoc/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :aoc)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
