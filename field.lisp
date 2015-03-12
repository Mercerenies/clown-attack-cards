(in-package :cardgame)

(defparameter *clown*
  (list 0 20))

(defparameter *player-hand*
  nil)

(defparameter *hand-size*
  8)

(defparameter *cards*
  nil)

(defparameter *time*
  (list 0 0))

(defun minutes-in-day (n)
  (* 3 (1+ n)))

(define-access time-days first)

(define-access time-minutes second)

(defun new-day (&optional (out t))
  (incf (time-days *time*))
  (setf (time-minutes *time*) 0)
  (when out
    (write-log "A new day begins")))

(defun check-clock ()
  (when (< (time-minutes *time*) 0)
    (setf (time-minutes *time*) 0))
  (when (>= (time-minutes *time*) (minutes-in-day (time-days *time*)))
    (new-day)
    t))

(define-access clown-meter first)

(define-access clown-cap second)

(defun clown-attack ()
  (setf (clown-meter *clown*) 0)
  (loop for X in *cards*
        never (on-clown-attacking X)
        finally (progn
                  (mapc #'on-clown-attacked *cards*)
                  (reset-field)
                  (write-log "Clown attack!" 'attack))))

(defun check-clown ()
  (when (< (clown-meter *clown*) 0)
    (setf (clown-meter *clown*) 0))
  (when (< (clown-cap *clown*) 0)
    (setf (clown-cap *clown*) 0))
  (when (>= (clown-meter *clown*) (clown-cap *clown*))
    (clown-attack)
    (setf (clown-meter *clown*) 0)
    t))

(defun advance-clock (n &optional (normal nil))
  (let ((final (reduce #'(lambda (X C) (on-clock-move C X normal))
                       *cards* :initial-value n)))
    (incf (time-minutes *time*) final)
    (check-clock)
    *time*))

(defun advance-clownometer (n &optional (normal nil))
  (let ((final (reduce #'(lambda (X C) (on-clownometer-move C X normal))
                       *cards* :initial-value n)))
    (incf (clown-meter *clown*) final)
    (check-clown)
    *clown*))

(defun advance-clownometer-capacity (n)
  (incf (clown-cap *clown*) n)
  (check-clown)
  *clown*)

(defun reset-clownometer ()
  (setf (clown-meter *clown*) 0)
  (check-clown)
  *clown*)

(defun check-triples-for-first ()
  (unless (null *player-hand*)
    (let ((C (car *player-hand*)))
      (when (and
             (>= (count (card-name C) *player-hand* :key #'card-name :test #'string=) 3)
             (card-triple C))
        (setq *player-hand*
              (remove (card-name C) *player-hand*
                      :key #'card-name :test #'string= :count 3))))))

(defun draw-card (&optional (out t))
  (when (< (length *player-hand*) *hand-size*)
      (set-in-front *player-hand* (next-card))
      (when out
        (write-log "Drew a card"))
      (check-triples-for-first)
      t))

(defun play-card (c)
  (when (or (card-stacks c) (null (find (type-of c) *cards* :key #'type-of)))
    (set-in-front *cards* c)
    (mapc #'(lambda (X) (on-play-card X c)) *cards*)))

(defun remove-from-play (c)
  (setq *cards* (remove c *cards* :test #'eq)))

(defun in-play (c)
  (find c *cards* :test #'eq))

(defun find-card (type)
  (find-if (lambda (X) (typep X type)) *cards*))

(defun use-card (c)
  (card-effect c)
  (setq *player-hand* (remove c *player-hand*)))

(defun end-of-turn ()
  (advance-clownometer 1 t)
  (advance-clock 1 t)
  (update-highscore)
  (save-highscore)
  (mapc #'on-turn-end *cards*))

(defun reset-field ()
  (setq *clown* (list 0 20))
  (setq *player-hand* nil)
  (setq *hand-size* 7)
  (setq *cards* nil)
  (setq *time* (list 0 0)))
