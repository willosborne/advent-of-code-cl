(defsystem "aoc"
  :version "0.1.0"
  :author ""
  :license ""
  :serial t
  :depends-on ("sqlite"
               "drakma"
               "fset"
               "alexandria"
               "named-readtables"
               "cl-interpol"
               "iterate"
               "parachute"
               "cl-ppcre"
               "maxpc"
               "arrow-macros"
               "rutils"
               "str"
               "cl-json")
  :components ((:module "src"
                :serial t
                :components
                ((:file "main")
                 (:file "cookies")
                 (:file "aoc-api")))
               ;; (:module "2019"
               ;;  :serial t
               ;;  :depends-on ("src")
               ;;  :components
               ;;  ((:file "day-1")
               ;;   (:file "day-2")
               ;;   (:file "day-3")
               ;;   (:file "day-4")
               ;;   (:file "day-5")))

               ;; (:module "2021"
               ;;  :serial t
               ;;  :depends-on ("src")
               ;;  :components
               ;;  ((:file "day-1")
               ;;   ))
               (:module "2023"
                :serial t
                :components
                ((:file "day-1")))
               )
  :description ""
  :in-order-to ((test-op (test-op "aoc/tests"))))

(asdf:defsystem :aoc/tests
  :description ""
  :author ""
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:aoc)
  :perform (test-op (o c) (uiop:symbol-call :aoc '#:run)))
;; (defsystem "aoc")

;; (defsystem "aoc/core"
;;   :depends-on ("sqlite" "drakma" "alexandria" "iterate" "str" "cl-json")
;;   :components ((:module "src"
;;                 :serial t
;;                 :components
;;                 ((:file "main")
;;                  (:file "cookies")
;;                  (:file "aoc-api"))))
;;   :description "Core integration package for the AOC website.")


;; (defsystem "aoc/year-2022"
;;   :description "asdf"
;;   :depends-on ("aoc/core")
;;   ;; :components ((:file "day-1"))
;;   :components ((:module "2022"
;;                 :serial t
;;                 :components
;;                 ((:file "day-1")
;;                  )))
;;   :in-order-to ((test-op (test-op "aoc/year-2022/tests"))))

;; (defsystem "aoc/year-2022/tests"
;;   :depends-on ("fiveam" "aoc/year-2022")
;;   :pathname "2022"
;;   :components ((:file "day-1-test")))

;; (defsystem "aoc/year-2023"
;;   :description "asdf"
;;   :depends-on ("aoc/core")
;;   ;; :components ((:file "day-1"))
;;   :pathname "2023"
;;   :serial t
;;   :components ((:file "day-1"))
;;   :in-order-to ((test-op (test-op "aoc/year-2023/tests"))))

;; (defsystem "aoc/year-2023/tests"
;;   :depends-on ("fiveam" "aoc/year-2023")
;;   :pathname "2023"
;;   :components ((:file "day-1-test")))


;; (defsystem "aoc/tests"
;;   :author ""
;;   :license ""
;;   :depends-on ("aoc"
;;                "rove")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "main"))))
;;   :description "Test system for aoc"
;;   :perform (test-op (op c) (symbol-call :rove :run c)))
