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

(defun get-intersections-steps (wires)
  (destructuring-bind (wire1 wire2) wires
    (intersections-steps wire1 wire2)))

(defun get-answer (input)
  (let ((intersections
          (->> input
            (get-wires)
            (get-intersections-steps))))
    (loop for (intersection steps) in intersections
          minimizing steps)))

(defun get-answer-2 ()
  (get-answer (aoc:get-input 2019 3)))

(defvar input-joe "R1004,U520,R137,D262,L403,U857,R50,U679,R788,D98,L717,D1,R367,U608,L125,U703,L562,D701,L718,U357,R742,D860,R557,D117,R950,U546,L506,U836,R951,D460,L38,U893,L1,D217,R262,D950,R239,U384,R971,D289,R323,U878,L525,U687,L831,U523,R94,D33,L879,D318,R633,D775,R879,D351,L120,D8,R31,U49,R328,D598,L380,D160,R261,D716,R459,U533,L444,U412,L326,U93,L193,D621,R236,U769,L319,D885,L559,U509,L62,U321,L667,D505,R556,U159,L5,U126,L262,D946,L168,U491,L56,D831,R926,U926,R562,D270,R785,U436,R852,D629,R872,U716,R549,U435,R462,U191,R318,U91,L637,D682,R647,D53,L789,D725,R312,D366,L287,U29,R85,D657,R88,U300,R795,U378,R800,D391,L594,U791,R205,U352,L510,D975,R47,D311,R319,U579,R214,D112,R996,U874,R328,D578,R37,U689,L543,U16,L580,D230,L714,D58,L580,D658,R218,U535,R149,U996,L173,D316,L90,D372,L364,U700,L60,D70,L250,U276,R580,U505,L682,U943,R336,U847,R810,U963,R874,D740,R732,D328,R926,D447,R638,D102,R696,U211,L594,D354,R384,U81,L884,U916,L168,U759,R631,D702,L598,D382,L647,U642,R537,U53,R897,U954,R263,U445,L41,D91,L51,D338,R219,U269,L689,D172,R627,D287,L440,D504,L253,D252,R815,D108,L282,U835,L243,U638,R910,D306,R755,D202,R69,D862,L537,D947,L180,D835,L111,U832,R939,D449,R180,U105,R892,D837,L153,U215,L695,U957,R923,U496,R608,U739,L711,U700,L838,D117,R479,U852,R795,D955,L386,D70,R728,D40,R580,U777,L877,U284,R414,D300,R105,D372,L317,D91,R653,U920,R956,D496,L543,D363,R374,D283,L696,U466,R467,D878,R660,U590,L962,U619,R991,U848,L648,D191,R459,U125,L998,U19,L214,U947,R188,U103,R916
L1008,U717,R288,D770,R270,U514,R109,D538,L719,U179,R466,D792,R421,U723,L22,U705,L284,U14,L478,U367,R727,U880,R620,D46,R377,U897,L731,U840,L910,D385,L257,U311,L596,D991,L668,D730,L707,D816,R47,U948,R84,D700,R299,U707,R261,D928,R358,D504,R309,U369,R931,U20,L940,U326,L362,D52,R98,D475,L907,D918,R931,D468,R279,D586,R592,U973,R753,D365,R694,U278,R934,U712,R441,U996,L989,D693,L211,D561,R105,D425,R53,U168,L451,U865,L585,D412,L857,U988,R724,U774,R295,U588,R329,D810,L698,D118,R277,U193,R309,U933,R186,D535,R409,U322,L849,U606,R590,U892,L542,D237,R475,D920,R679,U602,L477,D634,L988,D540,L323,U791,L375,U625,L621,U567,L943,U512,L239,D90,L66,U151,R83,U435,R612,D865,L177,U368,R326,U574,L241,U197,R499,U419,R297,U207,L311,D243,L559,D281,R513,U748,L884,U207,R71,D441,R133,D993,L4,D977,L669,U523,L564,U186,R477,U737,L685,U338,L456,U939,R774,U674,L97,D827,R237,D451,R618,D143,R750,U196,L559,D178,L693,D916,R334,U231,L651,U249,R620,U283,L387,U352,L915,U959,L693,U909,R320,U119,L617,U177,L993,D265,R667,U204,R59,D601,L579,U483,R155,D484,L44,D751,R915,U510,L552,U308,R505,U394,R585,U872,L617,U202,R928,U941,R235,U768,R666,D547,L244,D270,R353,D612,R384,U430,L685,D536,R103,U147,R794,D621,L52,U96,L557,D455,L635,D58,R265,U545,R938,D266,L173,U746,L672,D237,R286,U131,R487,U837,R394,D702,R49,U579,L699,U819,L448,D223,L982,D906,L397,U807,L737,D223,L791,D965,R436,U29,R908,D273,R194,U91,R232,U591,L336,D70,R467,U505,L341,U989,R278,U387,L442,U950,R487,D384,L534,D514,L433,U627,R381,U54,L847,U231,L590")
