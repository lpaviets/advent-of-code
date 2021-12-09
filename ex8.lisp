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
  "Suppose SOL is in order. Check whether the number NUM could be encoded
by ENCODING with the partial solution SOL"
  (let ((segs (aref +segments+ num)))
    (when (= (length encoding) (length segs))
      (and
       ;; Check that all activated wire of ENCODING are actually
       ;; linked to segments that should be "on" if this way the number
       ;; NUM
       (loop :for wire :in encoding ; x = sol[i] <-> wire i is controlling seg x
             :for seg = (nth wire sol)
             :always (or (not seg)
                         (member seg segs)))
       ;; Check that all the wires of SOL linked to segments that should
       ;; NOT be "on" are indeed not part of the encoding
       ;; (loop :for seg :in solution
       ;;       :for wire :from 0
       ;;       :always (when (member seg segs)
       ;;                 (member wire encoding)))
       ))))

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
