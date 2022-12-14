(in-package #:aoc2022/ex14)

(defparameter *source-location* '(0 500))
(defparameter *abyss-threshold* 0)

(defun parse-input (file)
  (setf *abyss-threshold* 0)
  (let ((input (read-file-as-sexprs file :parse (lambda (x) (substitute-if-not #\Space #'digit-char-p x))))
        (cave (make-array (list 200 1000) :initial-element nil)))
    (dolist (line input)
      (loop :for (y1 x1 y2 x2) :on line :by #'cddr
            :while y2
            :do (do-line (i j) (x1 y1) (x2 y2)
                  (setf (aref cave i j) t)
                  (setf *abyss-threshold* (max *abyss-threshold* x1 x2)))))
    (let ((floor-x (+ 2 *abyss-threshold*)))
      (do-line (i j) (floor-x 0) (floor-x 999)
        (setf (aref cave i j) t)))
    cave))

(defun cave-free-p (cave i j)
  (not (aref cave i j)))

(defun pos-below (i j)
  (let ((x (1+ i)))
    (list (list x j)                    ; below
          (list x (1- j))               ; below left
          (list x (1+ j))))) ; below-right

(defun place-sand (cave &optional with-floor)
  (loop :for (i j) = *source-location* :then next
        :for next = (find-if (lambda (pos)
                               (cave-free-p cave
                                            (first pos)
                                            (second pos)))
                             (pos-below i j))
        :while next
        :for in-abyss-p = (and (not with-floor) (> (first next) *abyss-threshold*))
        :when in-abyss-p
          :do (return nil)
        :finally (return (setf (aref cave i j) t))))

(defun place-until-abyss (cave)
  (loop :while (place-sand cave)
        :count 1))

(defun place-until-blocked-source (cave)
  (loop :while (and (apply 'cave-free-p cave *source-location*)
                    (place-sand cave t))
        :count 1))

(defun answer-ex-14-1 ()
  (place-until-abyss (parse-input "../inputs/input14")))

(defun answer-ex-14-2 ()
  (place-until-blocked-source (parse-input "../inputs/input14")))
