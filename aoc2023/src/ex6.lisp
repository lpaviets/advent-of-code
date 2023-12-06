(in-package #:aoc2023/ex6)

;; Equation:
;; - Total time T
;; - Time held H
;; Then distance is d = (T-H)H
;; Number of points above some threshold R:
;; (T-H)H > R  <=>  H^2 - TH + R < 0
;; Which means (as a function of H):
;; (T +- sqrt(T^2 - 4R))/2 are the root,

(defun count-solutions (time record)
  (let* ((disc (sqrt (- (* time time) (* 4 record))))
         (root1 (/ (- time (ceiling disc)) 2))
         (root2 (/ (+ time (ceiling disc)) 2))
         (gap (- (floor root2) (ceiling root1))))
    ;; Need to count the integers between those, excluding the roots !
    (if (integerp root1)
        (1- gap)
        (1+ gap))))

(defun answer-ex-6-1 ()
  (let ((params (read-file-as-lines "../inputs/input6.txt" :parse 'collect-integers-in-line)))
    (reduce '* (mapcar 'count-solutions (first params) (second params)))))

(defun answer-ex-6-2 ()
  (let ((params (read-file-as-lines "../inputs/input6.txt"
                                    :parse (lambda (line)
                                             (parse-integer (remove-if-not 'digit-char-p line))))))
    (count-solutions (first params) (second params))))
