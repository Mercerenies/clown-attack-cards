(in-package :cardgame)

(defgeneric on-clown-attacking (card)
  (:documentation "Triggered when the clown is about to attack. Returns whether the
                   clown attack should be cancelled."))

(defmethod on-clown-attacking ((card card))
  nil)

(defgeneric on-clown-attacked (card)
  (:documentation "Triggered after a successful clown attack, just before all of
                   the objects in play are removed."))

(defmethod on-clown-attacked ((card card))
  nil)

(defgeneric on-clownometer-move (card n normal)
  (:documentation "Triggered just before the Clownometer changes. Returns the new
                   amount to move the Clownometer by."))

(defmethod on-clownometer-move ((card card) n normal)
  (declare (ignore normal))
  n)

(defgeneric on-clock-move (card n normal)
  (:documentation "Triggered just before the clock changes. Returns the new
                   amount to move the clock by."))

(defmethod on-clock-move ((card card) n normal)
  (declare (ignore normal))
  n)

(defgeneric on-turn-end (card)
  (:documentation "Triggered at the end of each turn."))

(defmethod on-turn-end ((card card))
  nil)

(defgeneric on-play-card (card played)
  (:documentation "Triggered immediately after a card has been played."))

(defmethod on-play-card ((card card) (played card))
  nil)

(defgeneric is-food (card)
  (:documentation "Returns whether the given card represents an edible item."))

(defmethod is-food ((card card))
  nil)

(defgeneric on-food-eat (card)
  (:documentation "Triggered when the food is being eaten. Must be defined for all
                   objects which #'is-food returns true."))
