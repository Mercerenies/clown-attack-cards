(in-package :cardgame)

(defmacro for-turn-counter (type &optional sym &body reaction)
  (let* ((card (gensym))
         (rxn (or reaction `((remove-from-play ,card)))))
    `(symbol-macrolet ,(when sym (list (list sym card)))
                      (defmethod on-turn-end ((,card ,type))
                        (counter-decr ,card))
                      (defmethod on-timer ((,card ,type))
                        ,@rxn))))
