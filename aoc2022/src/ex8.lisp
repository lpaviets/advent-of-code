(in-package #:aoc2022/ex8)

(defun forest-height (forest)
  (array-dimension forest 0))

(defun forest-width (forest)
  (array-dimension forest 1))

(defun tree-at (i j forest)
  (aref forest i j))

(defun visible-tree-p (i j forest)
  (let ((height (tree-at i j forest)))
    (or (loop :for k :from 0 :below i
              :always (< (tree-at k j forest) height))
        (loop :for k :from (1+ i) :below (forest-height forest)
              :always (< (tree-at k j forest) height))
        (loop :for k :from 0 :below j
              :always (< (tree-at i k forest) height))
        (loop :for k :from (1+ j) :below (forest-width forest)
              :always (< (tree-at i k forest) height)))))

(defun inside-visible-trees-count (forest)
  (loop :for i :from 1 :below (1- (forest-height forest))
        :sum (loop :for j :from 1 :below (1- (forest-width forest))
                   :count (visible-tree-p i j forest))))

(defun outside-visible-trees-count (forest)
  (+ (* 2 (forest-height forest))
     (* 2 (forest-width forest))
     -4))

(defun scenic-score (i j forest)
  (let ((height (tree-at i j forest)))
    (* (loop :for k :downfrom (1- i) :to 0
             :count 1
             :while (< (tree-at k j forest) height))
       (loop :for k :from (1+ i) :below (forest-height forest)
             :count 1
             :while (< (tree-at k j forest) height))
       (loop :for k :downfrom (1- j) :to 0
             :count 1
             :while (< (tree-at i k forest) height))
       (loop :for k :from (1+ j) :below (forest-width forest)
             :count 1
             :while (< (tree-at i k forest) height)))))

(defun highest-scenic-score (forest)
  (let ((max-score 0))
   (do-array (i j x forest max-score)
     (setf max-score (max max-score (scenic-score i j forest))))))

(defun answer-ex-8-1 ()
  (let ((forest (read-file-as-array "../inputs/input8")))
    (+ (inside-visible-trees-count forest)
       (outside-visible-trees-count forest))))

(defun answer-ex-8-2 ()
  (let ((forest (read-file-as-array "../inputs/input8")))
    (highest-scenic-score forest)))
