(in-package #:aoc2021/ex14)

(defparameter *rules* (make-hash-table :test 'equal))

(defun parse-rules (list)
  (loop :for raw-rule :in list
        :for (rule insert) = (ppcre:split " -> " raw-rule)
        :do (setf (gethash rule *rules*) (char insert 0))))

(defun to-insert (char-a char-b)
  (let ((string (format nil "~a~a" char-a char-b)))
    (gethash string *rules*)))

(defun starting-chain (list)
  (coerce (car list) 'list))

(defun rule-step (chain)
  (loop :for (a b) :on chain
        :for x = (to-insert a b)
        :collect a
        :when x
          :collect x))

(defun char-to-index (char)
  (- (char-code char)
     (char-code #\A)))

(defun index-to-char (index)
  (code-char (+ index (char-code #\A))))

(defun frequencies (chain)
  (let ((array (make-array 26)))
    (loop :for c :in chain
          :do (incf (aref array (char-to-index c))))
    array))

(defun most-minus-less-frequent (frequencies)
  (loop :for count :across frequencies
        :maximize count :into max-freq
        :when (plusp count)
          :minimize count :into min-freq
        :finally (return (- max-freq min-freq))))

;;; Part 2
;; Lists get too big to do the same thing as in Part 1, simply increasing
;; the number of steps
;; Solution: Dynamic Programming !
;; The key remark is that endpoints of a string never change when inserting
;; elements, so we can deal indepently with each substring
;; Recombination is also easy
;; Another solution would be to do recursion + memoization, but it would
;; probably be harder to implement from scratch

(defparameter *chain-step-memo* (make-hash-table :test 'equal)
  "Hash table containing the number of occurences of each letter
in the n-th step starting from some chain
Keys are (A B N) where A and B are characters and N a positive integer
The associated value is an array, containing in position I the number of
occurences of the I-th letter in alphabetic order, in the N-th iteration
step, starting from the chain \"AB\"")

(defun init-hash-table ()
  (setf *chain-step-memo* (make-hash-table :test 'equal))
  (loop :for a :below 26 :do
          (loop :for b :below 26
                :for chain = (list (index-to-char a)
                                   (index-to-char b))
                :for freqs = (frequencies (rule-step chain))
                :do
                   (setf (gethash (append chain (list 0))
                                  *chain-step-memo*)
                         (frequencies chain))
                   (setf (gethash (append chain (list 1))
                                  *chain-step-memo*)
                         freqs))))

(defun add-freqs (array array-res common)
  (loop :for i :below 26
        :do (incf (aref array-res i) (aref array i))
        :finally (decf (aref array-res (char-to-index common)))))

(defun frequencies-chain-steps (chain steps)
  (symbol-macrolet ((res (gethash (append chain (list steps)) *chain-step-memo*)))
    ;; (format t "Chain: ~a and steps: ~a~%Already seen ? ~a~%~%" chain steps res)
    (if res
        res
        (loop :with freqs = (make-array 26)
              :for (a b) :on (rule-step chain)
              :when b
                :do (let ((subfreqs (frequencies-chain-steps (list a b) (1- steps))))
                      (add-freqs subfreqs freqs a))
              :finally
                 (incf (aref freqs (char-to-index (car chain)))) ; was decremented for no reason
                 (setf res freqs)
                 (return freqs)))))

(defun answer-ex-14-1 ()
  (let* ((list (read-file-as-lines "../inputs/input14.txt"))
         (start-chain (starting-chain list)))
    (parse-rules (cddr list))
    (let ((final-chain (loop :repeat 10
                             :for chain = (rule-step start-chain) :then (rule-step chain)
                             :finally (return chain))))
      (most-minus-less-frequent (frequencies final-chain)))))

(defun answer-ex-14-2 ()
  (let* ((list (read-file-as-lines "../inputs/input14.txt"))
         (start-chain (starting-chain list)))
    (parse-rules (cddr list))
    (init-hash-table)
    (most-minus-less-frequent (frequencies-chain-steps start-chain 40))))
