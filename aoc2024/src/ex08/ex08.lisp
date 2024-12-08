(in-package #:aoc2024/ex8)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-file-to-antennas (file)
  (let ((grid (read-file-as-array file))
        (antennas (make-hash-table :test 'eql)))
    (do-array (i j x grid (list grid antennas))
      (unless (char= x #\.)
        (push (list i j) (gethash x antennas))))))

;; (defun find-antinodes (antennas grid)
;;   (let ((antinodes (make-hash-table :test 'equal)))
;;     (do-hashvalues (positions antennas)
;;       (loop :for ((x1 y1) . rest-pos) :on positions
;;             :do (loop :for (x2 y2) :in rest-pos
;;                       :for dx = (- x2 x1)
;;                       :for dy = (- y2 y1)
;;                       :for antinode-1 = `(,(- x1 dx) ,(- y1 dy))
;;                       :for antinode-2 = `(,(+ x2 dx) ,(+ y2 dy))
;;                       :when (apply 'array-in-bounds-p grid antinode-1)
;;                         :do (setf (gethash antinode-1 antinodes) t)
;;                       :when (apply 'array-in-bounds-p grid antinode-2)
;;                         :do (setf (gethash antinode-2 antinodes) t))))
;;     antinodes))

(defun find-antinodes (antennas grid)
  (let ((antinodes (make-hash-table :test 'equal)))
    (do-hashvalues (positions antennas)
      (do-sequence-subsets ((p1 p2) (2 positions))
        (let* ((diff (mapcar '- p2 p1))
               (node-1 (mapcar '+ p2 diff))
               (node-2 (mapcar '- p1 diff)))
          (when (apply 'array-in-bounds-p grid node-1)
            (setf (gethash node-1 antinodes) t))
          (when (apply 'array-in-bounds-p grid node-2)
            (setf (gethash node-2 antinodes) t)))))
    antinodes))

;; (defun find-all-antinodes (antennas grid)
;;   (let ((antinodes (make-hash-table :test 'equal)))
;;     (do-hashvalues (positions antennas)
;;       (loop :for ((x1 y1) . rest-pos) :on positions
;;             :do (loop :for (x2 y2) :in rest-pos
;;                       :for dx = (- x2 x1)
;;                       :for dy = (- y2 y1)
;;                       :do (loop :for ax1 = x1 :then (- ax1 dx)
;;                                 :for ay1 = y1 :then (- ay1 dy)
;;                                 :for antinode-1 = (list ax1 ay1)
;;                                 :while (apply 'array-in-bounds-p grid antinode-1)
;;                                 :do (setf (gethash antinode-1 antinodes) t))
;;                           (loop :for ax2 = x2 :then (+ ax2 dx)
;;                                 :for ay2 = y2 :then (+ ay2 dy)
;;                                 :for antinode-2 = (list ax2 ay2)
;;                                 :while (apply 'array-in-bounds-p grid antinode-2)
;;                                 :do (setf (gethash antinode-2 antinodes) t)))))
;;     antinodes))

(defun find-all-antinodes (antennas grid)
  (let ((antinodes (make-hash-table :test 'equal)))
    (do-hashvalues (positions antennas)
      (do-sequence-subsets ((p1 p2) (2 positions))
        (let ((diff (mapcar '- p2 p1)))
          (loop :for node-1 = p1 :then (mapcar '- node-1 diff)
                :while (apply 'array-in-bounds-p grid node-1)
                :do (setf (gethash node-1 antinodes) t))
          (loop :for node-2 = p2 :then (mapcar '+ node-2 diff)
                :while (apply 'array-in-bounds-p grid node-2)
                :do (setf (gethash node-2 antinodes) t)))))
    antinodes))

(defun answer-ex-8-1 (file)
  (destructuring-bind (grid antennas)
      (read-file-to-antennas file)
    (hash-table-count (find-antinodes antennas grid))))

(defun answer-ex-8-2 (file)
  (destructuring-bind (grid antennas)
      (read-file-to-antennas file)
    (hash-table-count (find-all-antinodes antennas grid))))
