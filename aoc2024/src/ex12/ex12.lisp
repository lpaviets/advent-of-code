(in-package #:aoc2024/ex12)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-garden (file)
  (read-file-as-array file))

;; Way simpler solution than what I did: loop by lines, then by columns.
;; Keep track of the "current region" and add 1 when region changes.
(defun graph-edges-from-garden (garden)
  (make-graph-from-grid garden
                        :cost (lambda (p1 p2)
                                (if (char= (grid-at p1 garden)
                                           (grid-at p2 garden))
                                    1
                                    nil))))

(defun count-plot-borders (plot garden)
  (let* ((neighbours (grid-neighbours plot garden))
         (walls (- 4 (length neighbours))) ; outside the grid: not returned by
                                        ; `grid-neighbours'
         (neighbours-outside (loop :with plant = (grid-at plot garden)
                                   :for nghb :in (grid-neighbours plot garden)
                                   :count (char/= plant (grid-at nghb garden)))))
    (+ walls neighbours-outside)))

(defun garden-regions (garden)
  (let ((edges (graph-edges-from-garden garden))
        (all-plots nil))
    (do-array (i j x garden)
      (push (list i j) all-plots))
    (connected-components all-plots edges :test 'equalp)))

(defun perimeter (region garden)
  (let ((total 0))
    (do-hashkeys (plot region)
      (incf total (count-plot-borders plot garden)))
    total))

(defun area (region)
  (hash-table-count region))

;;; Part 2

;;; Idea of the computation:
;;; - A side <=> a corner
;;; - A 'out'-corner <=> two 'consecutive' sides adjacent to the exterior
;;; - A 'in'-corner  <=> two consecutive sides in the same region, but diagonal is not

(defun neighbour-in-region-p (plot region dir)
  (gethash (grid-pos-in-direction plot dir) region))

(defun count-out-corners (plot region)
  (loop :for (dir1 dir2) :on '(:up :right :down :left :up)
        :while dir2
        :count (and (not (neighbour-in-region-p plot region dir1))
                    (not (neighbour-in-region-p plot region dir2)))))

(defun count-in-corners (plot region)
  (loop :for (dir1 dir2) :on '(:up :right :down :left :up)
        :while dir2
        :count (and (neighbour-in-region-p plot region dir1)
                    (neighbour-in-region-p plot region dir2)
                    (not (neighbour-in-region-p (grid-pos-in-direction plot dir1) region dir2)))))

(defun count-corners (plot region)
  (+ (count-in-corners plot region)
     (count-out-corners plot region)))

(defun sides (region)
  (let ((sides 0))
    (do-hashkeys (plot region)
      (incf sides (count-corners plot region)))
    sides))

(defun answer-ex-12-1 (file)
  (let ((garden (parse-garden file)))
    (loop :for region :in (garden-regions garden)
          :sum (* (area region)
                  (perimeter region garden)))))

(defun answer-ex-12-2 (file)
  (let ((garden (parse-garden file)))
    (loop :for region :in (garden-regions garden)
          :sum (* (area region)
                  (sides region)))))
