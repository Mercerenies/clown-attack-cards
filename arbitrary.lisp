(in-package :cardgame)

; Borrowed from package :genie

(defclass arbitrary ()
  ((data :accessor arbitrary-props
         :initform nil
         :initarg :props
         :type list
         :documentation "An arbitrary property list suitable for use with assoc"))
  (:documentation "A class containing an arbitrary property list of data.
                   This class is intended to be inherited from in classes
                   that wish to have their data expandable at runtime."))

(defun get-data (arb key)
  "Get the data associated with a given key."
  (cdr (assoc key (arbitrary-props arb))))

(defun set-data (arb key value)
  "Set the data associated with a given key in the arbitrary instance,
   or create it and set it if the key does not exist."
  (let ((X (assoc key (arbitrary-props arb))))
    (if X
        (setf X value)
      (set-in-front (arbitrary-props arb) (cons key value)))))

(defsetf get-data set-data
  "Allows easy assignment to arbitrary data, even to keys that do not yet exist.")
