(in-package #:aoc2021/ex8)

(defconstant +segments+ (make-array 10 :initial-contents '((0 1 2 4 5 6)
                                                          (2 5)
                                                          (0 2 3 4 6)
                                                          (0 2 3 5 6)
                                                          (1 2 3 5)
                                                          (0 1 3 5 6)
                                                          (0 1 3 4 5 6)
                                                          (0 2 5)
                                                          (0 1 2 3 4 5 6)
                                                          (0 1 2 3 5 6))))

(defun parse-input (line)
  (let ((pos (search "|" line)))
    (ppcre:split " +" (subseq line (+ pos 2)))))

(defun unique-p (coding)
  (member (length coding) '(2 3 4 7)))

;;; Part 2

(defun char-to-int (word)
  (map 'list
       (lambda (x)
         (- (char-int x) (char-int #\a)))
       word))

(defun parse-input-2 (line)
  (let* ((pos (search "|" line))
         (goal (mapcar 'char-to-int
                       (ppcre:split " +" (subseq line (+ pos 2)))))
         (entries (mapcar 'char-to-int
                          (ppcre:split " +" (subseq line 0 (1- pos))))))
    (list entries goal)))

(defun compatible-p (num encoding sol)
  "Assume SOL in order. Check whether the number NUM could be encoded
by ENCODING with the partial solution SOL"
  (let ((segs (aref +segments+ num)))
    (when (= (length encoding) (length segs))
      ;; Check that all activated wire of ENCODING are actually
      ;; linked to segments that should be "on" if this way the number
      ;; NUM
      ;; Could do more tests to reduce the number of "wrong" guesses
      ;; Only important thing is to return NIL for a bad "full" solution,
      ;; and non-NIL for the correct one
      ;; Further improvements are only optimisations, not requirements
      (loop :for wire :in encoding ; x = sol[i] <-> wire i is controlling seg x
            :for seg = (nth wire sol)
            :always (or (not seg)
                        (member seg segs))))))

(defun compatible-nums (encoding sol)
  (loop :for i :upto 9
        :when (compatible-p i encoding sol)
          :collect i))

(defun guess (wire sol input) ;; sol is in the reverse order -> (w_2 w_1 w_0)
  ;; (format t "Guessing wire ~a~%partial sol = ~a~%~%" wire sol)
  (when (= 7 wire)
    (return-from guess sol))
  (dotimes (seg 7)
    (unless (member seg sol)
      (let ((new-sol (cons seg sol)))
        (when (check new-sol input)
          (let ((final-sol (guess (1+ wire) new-sol input)))
            (when final-sol
              (return final-sol))))))))

(defun check (sol input)
  "Check whether the partial solution SOL is compatible with all the
elements in INPUT"
  ;; (format t "Checking solution ~a on input ~a~%~%" sol input)
  (loop :with ord-sol = (reverse sol)
        :for encoding :in input
        :for num = (compatible-nums encoding ord-sol)
        :if num
          :collect num
        :else
          :do (return)))

(defun decode (encoding sol)
  (let ((segments-code (sort (loop :for wire :in encoding
                                   :collect (nth wire sol))
                             '<)))
    (loop :for i :upto 9
          :for segments-i = (aref +segments+ i)
          :thereis (and (equal segments-i segments-code)
                         i))))

(defun decode-goals (goals sol)
  (loop :for goal :in goals
        :for decode = (decode goal sol)
        :for result = decode :then (+ decode (* 10 result))
        :finally (return result)))

(defun answer-ex-8-1 ()
  (let ((list (read-file-as-lines "inputs/input8.txt" :parse 'parse-input)))
    (loop :for line :in list
          :sum (count-if 'unique-p line))))

(defun answer-ex-8-2 ()
  (let ((list (read-file-as-lines "inputs/input8.txt")))
    (loop :for line :in list
          :for (input goals) = (parse-input-2 line)
          :for sol = (reverse (guess 0 nil input))
          :sum (decode-goals goals sol))))

(defun alternative-answer-ex-8-2 ()
  "Bruteforce version with all permutations tested one by one"
  (let ((list (read-file-as-lines "inputs/input8.txt")))
    (loop :with perms = (permutations 6)
          :for line :in list
          :for (input goals) = (parse-input-2 line)
          :for sol = (loop :for perm :in perms
                           :thereis (and (check perm input)
                                         (reverse perm)))
          :sum (decode-goals goals sol))))

;;; Comparison:
;; Number of calls to the `check' function on my own input file:
;; Total for clever version: 32227
;; Total for brutef version: 552272

;; Bruteforce version seems more than 10* worse
;; More objective comparison with the `time' macro:

;; AOC2021/EX8> (time (answer-ex-8-2))
;; Evaluation took:
;;   0.059 seconds of real time
;;   0.061216 seconds of total run time (0.061216 user, 0.000000 system)
;;   [ Run times consist of 0.005 seconds GC time, and 0.057 seconds non-GC time. ]
;;   103.39% CPU
;;   207,192,294 processor cycles
;;   9,731,248 bytes consed

;; AOC2021/EX8> (time (alternative-answer-ex-8-2))
;; Evaluation took:
;;   0.255 seconds of real time
;;   0.258004 seconds of total run time (0.254774 user, 0.003230 system)
;;   [ Run times consist of 0.018 seconds GC time, and 0.241 seconds non-GC time. ]
;;   101.18% CPU
;;   876,856,936 processor cycles
;;   70,830,640 bytes consed
