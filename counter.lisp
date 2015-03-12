(in-package :cardgame)

(defclass counter ()
  ((countdown :accessor countdown
              :initarg :countdown
              :initform 0
              :type (integer 0 *))))

(defgeneric on-timer (counter))

(defun counter-add (c n)
  (incf (countdown c) n)
  (when (<= (countdown c) 0)
      (on-timer c)))

(defun counter-decr (c)
  (counter-add c -1))
