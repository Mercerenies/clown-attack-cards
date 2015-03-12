(in-package :cardgame)

; get-integer borrowed from :genie
(defun get-integer (start end &optional (invalid "Invalid input. Please try again. "))
  (loop for N = (read)
        until (typep N `(integer ,start ,end))
        do (format t invalid)
        finally (return N)))

(defun input-card-from (cards &key
                              (prompt "Input a card name: ")
                              (invalid "Nonexistent card. Try again. "))
  (format t prompt)
  (let ((IN (read-line)))
    (or (find IN cards :key (compose #'car #'option-name) :test #'string-equal)
        (input-card-from cards :prompt invalid :invalid invalid))))

(defun do-turn-text ()
  (format t "~%Clownometer ~A/~A~%Clock ~A~%Days ~A~%"
          (clown-meter *clown*)
          (clown-cap *clown*)
          (time-minutes *time*)
          (time-days *time*))
  (format t "In play: ~:[(None)~;~:*~{~A~^, ~}~]~%" (mapcar #'field-name *cards*))
  (format t "~%~{~A - ~A~%~}Deck~%Quit~%"
          (loop for X in *player-hand*
                append (list (card-name X) (card-desc X))))
  (let* ((lst (append *player-hand* (list *option-deck* *option-quit*)))
         (C (input-card-from lst)))
    (when (case C
                (*option-deck* (draw-card))
                (*option-quit* (return-from do-turn-text 'quit))
                (otherwise (prog1 t
                             (use-card C))))
      (end-of-turn))))

(defun run-game-in-text ()
  (reset-field)
  (init-dict)
  (load-highscore)
  (with-log-handler
   (loop until (eq (do-turn-text) 'quit))))
