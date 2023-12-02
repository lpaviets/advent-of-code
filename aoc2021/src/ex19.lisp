(in-package #:aoc2021/ex19)

(defparameter *scanners* nil)
(defparameter *distance-graph* nil)
(defparameter *sorted-distances* nil)
(defparameter *orientations* nil)

(defstruct matching
  (matches nil)
  (scan-a 0)
  (scan-b 1))

(defclass scanner ()
  ((num
    :reader num
    :initarg :num
    :type fixnum)
   (points
    :accessor points
    :initarg :points
    :type array)
   (orientation
    :accessor orientation
    :initarg :orientation
    :initform nil
    :type list)
   (pos
    :accessor pos
    :initarg pos
    :initform nil
    :type list)))

(defclass point ()
  ((scanner
    :reader scanner
    :initarg :scanner
    :type fixnum)
   (coords
    :reader coords
    :initarg :coords
    :type list)))

(defun make-point (coords &optional (scan 0))
  (make-instance 'point
                 :scanner scan
                 :coords coords))

(defun gen-orientations (n)
  (if (= 1 n)
      (list '(1) '(-1))
      (loop :with perms = (gen-orientations (1- n))
            :for perm :in perms
            :append
            (loop :repeat n
                  :for (beg end) = (list nil perm)
                    :then (list (cons (car end) beg)
                                (cdr end))
                  :append (list
                           (append beg (list (- n))
                                   end)
                           (append beg (list n)
                                   end))))))

(defun inv-orientation (orientation)
  (loop :for coord-num :from 0 :upto 2
        :collect (nth coord-num orientation)))

(defun parse-scanners (filename)
  (let ((scanners (with-open-file (in filename)
                    (loop
                      :while (read-line in nil nil) ; scanner name
                      :for i :from 0
                      :for scanner = (loop :for line = (read-line in nil nil)
                                           :while (and line
                                                       (not (string= "" line)))
                                           :for coords = (mapcar 'parse-integer
                                                                 (ppcre:split "," line))
                                           :collect
                                           (make-point coords
                                                       i))
                      :while scanner
                      :collect
                      (make-instance 'scanner
                                     :num i
                                     :points (make-array (length scanner)
                                                         :initial-contents scanner))))))

    (setf *scanners* (make-array (length scanners)
                                 :initial-contents scanners))))

(defmethod euclidean-dist-sq ((x list) (y list))
  (loop :for xi :in x
        :for yi :in y
        :sum (expt (- yi xi) 2)))

(defmethod euclidean-dist-sq ((x point) (y point))
  (euclidean-dist-sq (coords x) (coords y)))

(defun edge-length (scanner)
  (loop :with len = (length (points scanner))
        :with distance = (make-array (list len len))
        :for i :from 0 :below len
        :for u = (aref (points scanner) i) :do
          (loop :for j :from (1+ i) :below len
                :for v = (aref (points scanner) j)
                :for dist = (euclidean-dist-sq u v) :do
                            (setf (aref distance i j) dist
                                  (aref distance j i) dist))
        :finally (return distance)))

(defun all-edges-length ()
  (let ((len (length *scanners*)))
    (setf *distance-graph* (make-array len)))
  (loop :for scanner :across *scanners* :do
    (setf (aref *distance-graph* (num scanner))
          (edge-length scanner))))

(defun sort-distances (scanner)
  (let ((num (num scanner))
        distances)
    (do-array (i j x (aref *distance-graph* num))
      (when (< i j)
        (push (list x i j) distances)))
    (setf (aref *sorted-distances* num)
          (sort distances (lambda (x y) (< (first x) (first y)))))))

(defun all-sort-distances ()
  (let ((len (length *scanners*)))
    (setf *sorted-distances* (make-array len)))
  (loop :for scanner :across *scanners* :do
    (sort-distances scanner)))

(defun init-distances (filename)
  (parse-scanners filename)
  (all-edges-length)
  (all-sort-distances)
  (setf *orientations* (gen-orientations 3))
  (setf (orientation (aref *scanners* 0)) '(1 2 3)
        (pos (aref *scanners* 0)) '(0 0 0)))

(defmethod find-matching-distances ((num-a integer) (num-b integer))
  ;; Buggy if some distances in the SAME scanner appear several times
  ;; Should not continue loop as usual when da = db, because there
  ;; migh be e.g. other elements (in dist-b) equal to db so we
  ;; can't just move on without looking at them
  (let ((dist-a (aref *sorted-distances* num-a))
        (dist-b (aref *sorted-distances* num-b)))
    (loop :with len-a = (length dist-a)
          :with len-b = (length dist-b)
          :with da = dist-a
          :with db = dist-b
          :while (and da db)
          :if (= (first (car da))
                 (first (car db)))
            :collect (list (cdr (car da)) ; only the two points, not the distance
                           (cdr (car db)))
            :and :do
              (setf da (cdr da))
              (setf db (cdr db))
          :else :do
            (if (< (first (car da))
                   (first (car db)))
                (setf da (cdr da))
                (setf db (cdr db))))))

(defmethod find-matching-distances ((scanner-a scanner) (scanner-b scanner))
  (find-matching-distances (num scanner-a) (num scanner-b)))

(defun candidate-points-from-distances (distances)
  (let ((candidates (loop :for ((a b) (c d)) :in distances
                          :append (list a b) :into candidates
                          :finally (return (remove-duplicates candidates)))))
    (loop :for x :in candidates
          :for matches = (loop :for ((a b) (c d)) :in distances
                               :if (or (= a x) (= b x))
                                 :append (list c d) :into dup-matches
                               :finally (return (remove-duplicates dup-matches)))
          :collect (list x matches))))

(defun find-matching-subgraph (distances candidates &optional partial-matching)
  (cond
    ((null candidates)
     (and (<= 12 (length (matching-matches partial-matching)))
          partial-matching))
    ((< 12 (length candidates))
     (let* ((len (length candidates))
            (avoid-indices (sublists-length (range len) (- len 12))))
       (loop :for indices :in avoid-indices
             :for subcandidates = (loop :for i :from 0
                                        :for x :in candidates
                                        :unless (member i indices)
                                          :collect x)
             :for new-matching = (find-matching-subgraph distances
                                                         subcandidates
                                                         partial-matching)
             :thereis (and new-matching
                           (matching-matches new-matching)
                           (orientation-from-matching new-matching)
                           new-matching))))
    (t (loop :with x-cands = (car candidates)
             :with x = (first x-cands)
             :for y :in (append (second x-cands) (list nil))
             :for new-matches =  (if y
                                     (cons (cons x y)
                                           (matching-matches partial-matching))
                                     (matching-matches partial-matching))
             :for new-matching = (make-matching :matches new-matches
                                                :scan-a (matching-scan-a partial-matching)
                                                :scan-b (matching-scan-b partial-matching))
             :thereis (and (correct-matching-no-orientation distances new-matches)
                           (orientation-from-matching new-matching)
                           (find-matching-subgraph distances
                                                   (cdr candidates)
                                                   new-matching))))))

(defmethod find-large-matching-subgraph ((num-a integer) (num-b integer))
  (let* ((distances (find-matching-distances num-a num-b))
         (candidates (candidate-points-from-distances distances)))
    (when (<= 12 (length candidates))
      ;; (format t "Potential overlap between ~a and ~a (~a candidates: ~a)~%"
      ;;         num-a num-b (length candidates) candidates)
      (find-matching-subgraph distances
                              candidates
                              (make-matching :matches nil
                                             :scan-a num-a
                                             :scan-b num-b)))))

(defmethod correct-matching-no-orientation (distances (matching list))
  "MATCHING is an alist, the elements of which are (U . V) where U is a
vertex from the first graph, V from the second one"
  (when (loop :for ((a b) (c d)) :in distances
            :always (or (or (not (assoc a matching))
                            (not (assoc b matching))) ; not both matches
                        (and (= (cdr (assoc a matching)) c)
                             (= (cdr (assoc b matching)) d)) ; a <-> c and b <-> d
                        (and (= (cdr (assoc a matching)) d)
                             (= (cdr (assoc b matching)) c))))  ; a <-> d and b <-> c
    matching))

(defmethod correct-matching-no-orientation (distances (matching matching))
  (correct-matching-no-orientation distances (matching-matches matching)))

(defun relative-oriented-distance (point-a point-b orientation)
  (assert (= (scanner point-a) (scanner point-b)))
  (with-accessors ((coords-a coords) (num num) (scanner scanner)) point-a
    (with-accessors ((coords-b coords)) point-b
      (let ((dist (mapcar '- coords-b coords-a)))
        (loop :for coord-num :from 1 :upto 3
              :for pos = (or (position coord-num orientation)
                             (position (- coord-num) orientation))
              :collect (* (signum (nth pos orientation))
                          (nth pos dist)))))))

(defmethod correct-matching-orientation ((matching matching) orientation)
  (loop :with scan-a = (aref *scanners* (matching-scan-a matching))
        :with scan-b = (aref *scanners* (matching-scan-b matching))
        :with matches = (matching-matches matching)
        :for rest = matches :then (cdr rest)
        :while (cdr rest)
        :for (a . b) = (car matches)
        :for pa = (aref (points scan-a) a)
        :for pb = (aref (points scan-b) b)
        :always
        (loop :for (x . y) :in (cdr rest)
              :for px = (aref (points scan-a) x)
              :for py = (aref (points scan-b) y)
              :always (equal
                       (relative-oriented-distance pa px (orientation scan-a))
                       (relative-oriented-distance pb py orientation)))))

(defun orientation-from-matching (matching)
  (loop :for orientation :in *orientations*
        :thereis (and (correct-matching-orientation matching orientation)
                      orientation)))

(defun absolute-pos (point)
  (let ((scanner (aref *scanners* (scanner point))))
    (mapcar '+
            (pos scanner)
            (relative-oriented-distance (make-point '(0 0 0)
                                                    (num scanner))
                                        point
                                        (orientation scanner)))))

(defun position-from-matching (matching)
  (let ((scan-a (aref *scanners* (matching-scan-a matching)))
        (scan-b (aref *scanners* (matching-scan-b matching)))
        (ref-points (car (matching-matches matching))))
    (let ((point-a (aref (points scan-a) (car ref-points)))
          (point-b (aref (points scan-b) (cdr ref-points))))
      (let ((absolute-pos (absolute-pos point-a))
            (absolute-dist-to-b
              (relative-oriented-distance (make-point '(0 0 0)
                                                      (num scan-b))
                                          point-b
                                          (orientation scan-b))))
        (mapcar '- absolute-pos absolute-dist-to-b)))))

(defun solve-orientation-and-positions (to-visit)
  (loop :while to-visit :do
    (loop
      :with scan-a = (pop to-visit)
      :for scan-b :across *scanners*
      :for num-a = scan-a
      :for num-b = (num scan-b)
      :for matching = (and (/= num-a num-b)
                           (not (pos scan-b))
                           (find-large-matching-subgraph num-a num-b))
      :when (and matching
                 (matching-matches matching)
                 (let ((or (orientation-from-matching matching)))
                   (and or (setf (orientation scan-b) or)))
                 (let ((pos (position-from-matching matching)))
                   (and pos (setf (pos scan-b) pos))))
        :do (push (num scan-b) to-visit))))

(defun get-all-points ()
  (let ((table (make-hash-table :test 'equal)))
    (loop :for scan :across *scanners* :do
      (loop :for point :across (points scan)
            :for pos = (absolute-pos point) :do
              (setf (gethash pos table) t)))
    table))

(defun count-total-points ()
  (hash-table-count (get-all-points)))

;;; Part 2

(defmethod manhattan-distance ((x list) (y list))
  (apply '+ (mapcar (lambda (a b) (abs (- b a))) x y)))

(defmethod manhattan-distance ((x point) (y point))
  (manhattan-distance (coords x) (coords y)))

(defun max-distance-scanners ()
  (loop :for scan-a :across *scanners*
        :maximize
        (loop :for scan-b :across *scanners*
              :maximize (manhattan-distance (pos scan-a) (pos scan-b)))))

(defun answer-ex-19-1 ()
  (init-distances "../inputs/input19.txt")
  (solve-orientation-and-positions '(0))
  (count-total-points))

(defun answer-ex-19-2 ()
  (init-distances "../inputs/input19.txt")
  (solve-orientation-and-positions '(0))
  (max-distance-scanners))
