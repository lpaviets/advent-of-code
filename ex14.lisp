(in-package #:aoc2021/ex14)

(defparameter *rules* (make-hash-table :test 'equal))
(defparameter *testlist* '("NNCB"
                           ""
                           "CH -> B"
                           "HH -> N"
                           "CB -> H"
                           "NH -> C"
                           "HB -> C"
                           "HC -> B"
                           "HN -> C"
                           "NN -> C"
                           "BH -> H"
                           "NC -> B"
                           "NB -> B"
                           "BN -> B"
                           "BB -> N"
                           "BC -> B"
                           "CC -> N"
                           "CN -> C"))

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

(defun frequencies (chain)
  (let ((array (make-array 26)))
    (loop :for c :in chain
          :for int-c = (- (char-code c)
                          (char-code #\A))
          :do (incf (aref array int-c)))
    array))

(defun most-minus-less-frequent (frequencies)
  (loop :for i :below 26
        :for count = (aref frequencies i)
        :maximize count :into max-freq
        :when (plusp count)
          :minimize count :into min-freq
        :finally (return (- max-freq min-freq))))

(defun answer-ex-14-1 ()
  (let* ((list (read-file-as-lines "inputs/input14.txt"))
         (start-chain (starting-chain list)))
    (parse-rules (cddr list))
    (let ((final-chain (loop :repeat 11
                             :for chain = start-chain :then (rule-step chain)
                             :finally (return chain))))
      (most-minus-less-frequent (frequencies final-chain)))))

(defun answer-ex-14-2 ()
  (let* ((list (read-file-as-lines "inputs/input14.txt"))
         (start-chain (starting-chain list)))
    (parse-rules (cddr list))
    (let ((final-chain (loop :repeat 41 ;;; Doesn't work, lists are too big
                             :for chain = start-chain :then (rule-step chain)
                             :do (print (length chain))
                             :finally (return chain))))
      (most-minus-less-frequent (frequencies final-chain)))))
