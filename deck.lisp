(in-package :cardgame)

(defclass card (arbitrary)
  ((name :accessor card-name
         :initform ""
         :initarg :name
         :type string)
   (desc :accessor card-desc
         :initform ""
         :initarg :desc
         :type string)
   (stacks :accessor card-stacks
           :initform t
           :initarg :stacks
           :type boolean)))

(defparameter *dictionary*
  nil)

(defgeneric field-name (card)
  (:documentation "The display name of the current state of the object when it
                   is in the field."))

(defmethod field-name ((card card))
  (card-name card))

(defun next-card ()
  (let* ((sum (reduce #'+ *dictionary* :key #'car :initial-value 0))
         (rnd (random sum)))
    (loop for N = rnd then (- N P)
          for (P . C) in *dictionary*
          while (>= N P)
          finally (return (eval C)))))

(defun add-to-dict (n x)
  (set-front-if-not *dictionary* (cons n x) :test (compose #'card-name #'cdr)))

(defgeneric card-image (c)
  (:documentation "Returns the index in the PNG resource of the card, or NIL
                   if none."))

(defmethod card-image ((c card))
  nil)

(defgeneric card-effect (c)
  (:documentation "Performs the card effect once. REQUIRED for all card instances."))

(defgeneric card-triple (c)
  (:documentation "Performs the card triple effect, returning T if an effect was
                   performed. If not included, defaults to returning NIL."))

(defmethod card-triple ((c card))
  nil)
