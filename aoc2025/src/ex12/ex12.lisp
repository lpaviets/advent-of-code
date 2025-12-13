(in-package #:aoc2025/ex12)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-present (present)
  ;; First line is of the form NUM:, the present "block" is made out of the
  ;; remaining lines
  (let ((width (length (second present)))
        (height (length (cdr present))))
    (make-array (list height width) :initial-contents (cdr present))))

(defun read-room (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer width height) presents)
      ("(\\d+)x(\\d+): (.+)" line)
    (list width height (mapcar 'parse-integer (cl-ppcre:split " " presents)))))

(defun read-all-rooms (rooms)
  (mapcar 'read-room rooms))

(defun read-all-presents (file)
  (multiple-value-bind (presents rooms)
      (loop :for present-or-room :in (read-file-as-lines-blocks file)
            :for line = (car present-or-room)
            ;; Dispatch correct parsing function according to first line
            :if (char= (char line (1- (length line))) #\:)
              :collect (read-present present-or-room) :into presents
            :else
              :collect (read-all-rooms present-or-room) :into rooms
            :finally (return (values presents rooms)))
    (let ((presents-size (make-array (length presents))))
      (loop :for i :from 0
            :for present :in presents
            :do (setf (aref presents-size i)
                      (cons present (grid-apply-as-sequence present 'count #\#))))
      (values presents-size (car rooms)))))

(defun can-never-fit-p (presents room)
  (destructuring-bind (w h required) room
    (let ((area-available (* w h))
          (area-required (loop :for count :in required
                               :for (present . size) :across presents
                               :sum (* count size))))
      (< area-available area-required))))

(defun can-definitely-fit-p (presents room)
  (multiple-value-bind (max-w max-h)
      (loop :for (present . size) :across presents
            :for (h w) = (array-dimensions present)
            :maximize h :into max-h
            :maximize w :into max-w
            :finally (return (values max-w max-h)))
    (destructuring-bind (w h required)
        room
      (let ((number-available (* (truncate w max-w) (truncate h max-h)))
            (number-required (reduce '+ required)))
        (<= number-required number-available)))))

(defun smart-can-fit-p (presents room)
  (declare (ignore presents room))
  (error "This is way too hard !"))

(defun try-fit-p (presents room)
  (cond
    ((can-never-fit-p presents room) nil)
    ((can-definitely-fit-p presents room) t)
    (t (smart-can-fit-p presents room))))

(defun answer-ex-12-1 (file)
  (multiple-value-bind (presents rooms)
      (read-all-presents file)
    (loop :for room :in rooms
          :count (try-fit-p presents room))))

(defun answer-ex-12-2 (file))
