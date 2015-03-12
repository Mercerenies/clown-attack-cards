(in-package :cardgame)

(defcard clown-card
  "Clown" "Increase the Clownometer (TRIPLE: Triggers a clown attack)"
  :image 0
  :effect (progn (write-log "Advanced the Clownometer")
                 (advance-clownometer 2))
  :triple (progn (write-log "Triple clown! The clown attacked!")
                 (clown-attack)
                 t))

(defcard no-clown-card
  "Anticlown" "Decrease the Clownometer (TRIPLE: Resets the Clownometer to 0)"
  :image 1
  :effect (progn (write-log "Decreased the Clownometer")
                 (advance-clownometer -4))
  :triple (progn (write-log "Triple Anticlown! The clown retreated!")
                 (reset-clownometer)
                 t))

(defcard nocturne-card
  "Vitamin" "Increase the Clownometer's capacity"
  :image 2
  :effect (progn (write-log "Increased Clownometer cap")
                 (advance-clownometer-capacity 2)))

(defcard dadio-card
  "Brick Wall" "Remains in play after activation, blocking the next clown attack; ~
                does not stack"
  :image 3
  :symbol C
  :effect (progn (write-log "A brick wall appeared")
                 (play-card C))
  :stacks nil)

(defmethod on-clown-attacking ((card dadio-card))
  (unless (find-card 'hayman-card)
    (remove-from-play card)
    (write-log "Clown attack blocked by a brick wall")
    t))

(defcard detpix-card
  "Ballerina" "Distracts the clown for a few turns, halting the Clownometer's ~
               normal progression"
  :symbol C
  :image 4
  :effect (progn (write-log "A ballerina was hired to distract the clown")
                 (play-card C))
  :class-parents (counter)
  :class-initargs (:countdown 3))

(defmethod field-name ((card detpix-card))
  (format nil "Ballerina (~A)" (countdown card)))

(for-turn-counter detpix-card)

(defmethod on-clownometer-move ((counter detpix-card) n normal)
  (if normal
      0
    n))

(defcard elevclock-card
  "Hourglass" "Advance the clock forward several minutes"
  :image 5
  :effect (progn (write-log "The sands of time have shifted forward")
                 (advance-clock 10)))

; TODO Make his triple effect more powerful
(defcard snailman-card
  "Snail" "Summon a magic snail to crawl over to the Clownometer and decrease ~
           it (TRIPLE: Summons a super snail that resets the Clownometer on arrival)"
  :symbol C
  :image (if (snail-super C) 7 6)
  :class-options ((super :accessor snail-super
                         :initform nil))
  :class-parents (counter)
  :class-initargs (:countdown 10)
  :effect (progn (write-log "A magic snail crawled into play")
                 (play-card C))
  :triple (progn (write-log "Triple Snail! A super snail crawled into play!")
                 (setf (snail-super C) t)
                 (play-card C)
                 t))

(defmethod field-name ((card snailman-card))
  (format nil "~:[Snail~;Super Snail~] (~A)" (snail-super card) (countdown card)))

(for-turn-counter snailman-card card
                  (if (snail-super card)
                      (progn (write-log "The super snail reset the Clownometer")
                             (reset-clownometer))
                    (progn (write-log "The magic snail decreased the Clownometer")
                           (advance-clownometer -7)))
                  (remove-from-play card))

(defcard myst-card
  "Rock" "Throw the rock at the clown, confusing him; all increases and ~
          decreases to the Clownometer are inverted for a short time"
  :symbol C
  :image 8
  :effect (progn (write-log "The clown was hit with a rock")
                 (play-card C))
  :class-parents (counter)
  :class-initargs (:countdown 3))

(defmethod field-name ((card myst-card))
  (format nil "Rock (~A)" (countdown card)))

(for-turn-counter myst-card)

(defmethod on-clownometer-move ((counter myst-card) n normal)
  (declare (ignore normal))
  (- n))

(defcard lune-card
  "Dealer" "Draw two cards"
  :image 9
  :effect (let ((*hand-size* (1+ *hand-size*)))
            (write-log "Drew two cards")
            (and (draw-card nil)
                 (draw-card nil))))

(defcard fugue-card
  "Stamp" "Advance the clock one minute (TRIPLE: Double the rate of movement of ~
           clock for several turns)"
  :symbol C
  :image 10
  :effect (progn (write-log "Stamped the clock")
                 (advance-clock 1))
  :triple (progn (write-log "Triple Stamp! The clock is racing now!")
                 (play-card C)
                 t)
  :class-parents (counter)
  :class-initargs (:countdown 12))

(defmethod field-name ((card fugue-card))
  (format nil "Stamp (~A)" (countdown card)))

(for-turn-counter fugue-card)

(defmethod on-clock-move ((counter fugue-card) n normal)
  (declare (ignore normal))
  (if (> n 0)
      (* n 2)
    n))

(defcard yal-card
  "Boyfriend" "Stays in play for ten turns, waiting for girlfriend; resets the ~
               Clownometer if he finds her; does not stack"
  :symbol C
  :image 11
  :effect (progn (write-log "A lonely boyfriend appeared")
                 (play-card C))
  :stacks nil
  :class-parents (counter)
  :class-initargs (:countdown 10))

(defmethod field-name ((card yal-card))
  (format nil "Boyfriend (~A)" (countdown card)))

(for-turn-counter yal-card)

(defcard tsuka-card
  "Girlfriend" "Stays in play for ten turns, waiting for boyfriend; resets the ~
                Clownometer if she finds him; does not stack"
  :symbol C
  :image 12
  :effect (progn (write-log "A lonely girlfriend appeared")
                 (play-card C))
  :stacks nil
  :class-parents (counter)
  :class-initargs (:countdown 10))

(defmethod field-name ((card tsuka-card))
  (format nil "Girlfriend (~A)" (countdown card)))

(for-turn-counter tsuka-card)

(defmethod on-play-card ((card tsuka-card) (played yal-card))
  (effect-friendship card played))

(defmethod on-play-card ((card yal-card) (played tsuka-card))
  (effect-friendship card played))

(defun effect-friendship (c1 c2)
  (write-log "The boyfriend and girlfriend were reunited")
  (write-log "The power of love reset the Clownometer")
  (reset-clownometer)
  (remove-from-play c1)
  (remove-from-play c2))

(defcard lan-card
  "Wristwatch" "Decrease the Clownometer slightly ~
                (TRIPLE: Advance three days immediately)"
  :image 13
  :effect (progn (write-log "Sent the clown back in time")
                 (advance-clownometer -1))
  :triple (progn (write-log "Triple Wristwatch! Three days have passed!")
                 (loop for x from 1 to 3
                       do (new-day nil))
                 t))

(defcard lukan-card
  "Wizard" "Double all increases and decreases to the Clownometer for a ~
            few turns (TRIPLE: Decreases the Clownometer's capacity)"
  :symbol C
  :image 14
  :effect (progn (write-log "A magic wizard enchanted the Clownometer")
                 (play-card C))
  :triple (progn (write-log "Triple Wizard! Angry wizards united to boost the clown!")
                 (advance-clownometer-capacity -4))
  :class-parents (counter)
  :class-initargs (:countdown 5))

(defmethod field-name ((card lukan-card))
  (format nil "Wizard (~A)" (countdown card)))

(for-turn-counter lukan-card)

(defmethod on-clownometer-move ((card lukan-card) n normal)
  (declare (ignore normal))
  (* n 2))

(defcard anurag-card
  "Viking" "Summon a viking in preparation for an attack"
  :symbol C
  :image 15
  :effect (progn (write-log "A viking appeared")
                 (play-card C)))

(defcard magnus-card
  "Longship" "Rally the vikings and attack, decreasing the Clownometer proportionally ~
              to the number of vikings (TRIPLE: Attack with triple effect)"
  :image 16
  :effect (progn (write-log "The vikings rallied and attacked")
                 (effect-viking -2))
  :triple (progn (write-log "Triple Longship! The vikings destroyed the clown!")
                 (effect-viking -6)
                 t))

(defun effect-viking (magn)
  (flet ((is-viking (X) (typep X 'anurag-card)))
        (let ((cnt (count-if #'is-viking *cards*)))
          (advance-clownometer (* cnt magn)))
        (setq *cards*
              (remove-if #'is-viking *cards*))))

(defcard alice-card
  "Subwoofer" "Deafen the clown for a few turns, negating increases or decreases ~
               to the Clownometer other than the normal progression"
  :symbol C
  :image 17
  :effect (progn (write-log "The clown was deafened by the deep bass")
                 (play-card C))
  :class-parents (counter)
  :class-initargs (:countdown 5))

(defmethod field-name ((card alice-card))
  (format nil "Subwoofer (~A)" (countdown card)))

(for-turn-counter alice-card)

(defmethod on-clownometer-move ((card alice-card) n normal)
  (if normal
      n
    0))

(defcard hayman-card
  "Magic Barrier" "Erect a magic barrier to briefly deflect all clown attacks"
  :symbol C
  :image 18
  :effect (progn (write-log "A magic barrier was erected")
                 (play-card C))
  :class-parents (counter)
  :class-initargs (:countdown 3))

(defmethod field-name ((card hayman-card))
  (format nil "Magic Barrier (~A)" (countdown card)))

(for-turn-counter hayman-card)

(defmethod on-clown-attacking ((card hayman-card))
  (write-log "The magic barrier deflected a clown attack")
  t)

(defcard pear-card
  "Pear" "Summon a pear, which when eaten decreases the Clownometer"
  :symbol C
  :image 19
  :effect (progn (write-log "A pear appeared")
                 (play-card C)))

(defmethod is-food ((card pear-card))
  t)

(defmethod on-food-eat ((card pear-card))
  (write-log "Ate a pear; Clownometer was decreased")
  (advance-clownometer -4))

(defcard potato-card
  "Potato" "Summon a potato, which when eaten advances the clock"
  :symbol C
  :image 20
  :effect (progn (write-log "A potato appeared")
                 (play-card C)))

(defmethod is-food ((card potato-card))
  t)

(defmethod on-food-eat ((card potato-card))
  (write-log "Ate a potato; time passed")
  (advance-clock 3))

(defcard steak-card
  "Steak" "Summon a steak, which when eaten increases the Clownometer capacity"
  :symbol C
  :image 21
  :effect (progn (write-log "A steak appeared")
                 (play-card C)))

(defmethod is-food ((card steak-card))
  t)

(defmethod on-food-eat ((card steak-card))
  (write-log "Ate a steak; you got stronger")
  (advance-clownometer-capacity 1))

(defcard feast-card
  "Feast" "Eat the most recently played food item (TRIPLE: Have a feast, passing ~
           several days based on the number of food items in play)"
  :image 22
  :effect (progn (let ((item (find-if #'is-food *cards*)))
                   (if item
                       (progn (on-food-eat item)
                              (remove-from-play item))
                     (write-log "Couldn't find anything to eat"))))
  :triple (progn (let ((days (count-if #'is-food *cards*)))
                   (format-log
                    "Triple Feast! All the food was eaten! Advanced ~A day~:p!"
                    days)
                   (loop for x from 1 to days
                         do (new-day nil))
                   (setq *cards* (remove-if #'is-food *cards*))
                   t)))

(defcard merc-card
  "Dragon" "Discard your entire hand to decrease the Clownometer proportional to ~
            the number of cards discarded (TRIPLE: Draw three cards)"
  :symbol C
  :image 23
  :effect (progn (write-log "Discarded your hand; the dragon's fire spread to the clown")
                 (let ((s (length *player-hand*)))
                   (advance-clownometer (* -2 (1- s))))
                 (setq *player-hand* nil))
  :triple (progn (write-log "Triple Dragon! Drew three cards!")
                 (setq *player-hand*
                       (remove-if #'(lambda (X) (typep X 'merc-card)) *player-hand*
                                  :count 3))
                 (loop for i from 1 to 3
                       do (draw-card nil))
                 (setq *player-hand* (append (list C C C) *player-hand*))
                 t))

(defcard mn-card
  "Sniper" "Shoot the clown, decreasing the Clownometer greatly but spoiling ~
            any food items currently in play"
  :image 24
  :effect (progn (write-log "Shot the clown; Clownometer decreased but food spoiled")
                 (advance-clownometer -10)
                 (setq *cards*
                       (remove-if #'is-food *cards*))))

(defcard life-card
  "Elixir" "Summon elixir; any cards played while elixir is in play will stay in ~
            play twice as long"
  :symbol C
  :image 25
  :effect (progn (write-log "A vial of elixir appeared")
                 (play-card C))
  :class-parents (counter)
  :class-initargs (:countdown 2))

(defmethod field-name ((card life-card))
  (format nil "Elixir (~A)" (countdown card)))

(for-turn-counter life-card)

(defmethod on-play-card ((card life-card) (played counter))
  (unless (eq card played) ; Elixirs do not power themselves
    (format-log "The ~(~A~) drank elixir" (card-name played))
    (setf (countdown played) (* 2 (countdown played)))))

(defcard duck-card
  "Duck" "Put a duck in the pond; if there are ten ducks, the Clownometer is reset ~
          and ten days pass (TRIPLE: Rally the ducks and reset the Clownometer)"
  :symbol C
  :image 26
  :effect (progn (write-log "A duck migrated to the pond")
                 (play-card C))
  :triple (progn (write-log "Triple Duck! The Clownometer was reset!")
                 (reset-clownometer)
                 (setq *cards*
                       (remove-if #'(lambda (X) (typep X 'duck-card)) *cards*))
                 t))

(defmethod on-play-card ((card duck-card) (played duck-card))
  (when (and (eq card played)
             (>= (count-if #'(lambda (X) (typep X 'duck-card)) *cards*) 10))
    (write-log "Tenth duck! The ducks destroyed the clown and passed the time!")
    (reset-clownometer)
    (loop for i from 1 to 10
          do (new-day nil))
    (setq *cards*
          (remove-if #'(lambda (X) (typep X 'duck-card)) *cards*))))

(defun init-dict ()
  (setq *dictionary* (list
                      (cons 50 '(make-instance 'clown-card))
                      (cons 27 '(make-instance 'no-clown-card))
                      (cons 14 '(make-instance 'nocturne-card))
                      (cons  2 '(make-instance 'dadio-card))
                      (cons 20 '(make-instance 'detpix-card))
                      (cons 21 '(make-instance 'elevclock-card))
                      (cons 20 '(make-instance 'snailman-card))
                      (cons 10 '(make-instance 'myst-card))
                      (cons 25 '(make-instance 'lune-card))
                      (cons 23 '(make-instance 'fugue-card))
                      (cons 23 '(make-instance 'yal-card))
                      (cons 10 '(make-instance 'tsuka-card))
                      (cons 22 '(make-instance 'lan-card))
                      (cons 20 '(make-instance 'lukan-card))
                      (cons 27 '(make-instance 'anurag-card))
                      (cons  9 '(make-instance 'magnus-card))
                      (cons 21 '(make-instance 'alice-card))
                      (cons  2 '(make-instance 'hayman-card))
                      (cons 18 '(make-instance 'pear-card))
                      (cons 18 '(make-instance 'potato-card))
                      (cons 18 '(make-instance 'steak-card))
                      (cons 29 '(make-instance 'feast-card))
                      (cons  8 '(make-instance 'merc-card))
                      (cons  9 '(make-instance 'mn-card))
                      (cons 16 '(make-instance 'life-card))
                      (cons 22 '(make-instance 'duck-card)))))
