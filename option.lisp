(in-package :cardgame)

(defparameter *option-deck* '*option-deck*)

(defparameter *option-quit* '*option-quit*)

(defgeneric option-name (opt))

(defmethod option-name ((opt card))
  (list (card-name opt) (card-desc opt)))

(defmethod option-name ((opt (eql *option-deck*)))
  (list "Deck" "Draw a card from the deck"))

(defmethod option-name ((opt (eql *option-quit*)))
  (list "Quit" "Exit the game entirely"))
