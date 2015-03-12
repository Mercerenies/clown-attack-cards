(in-package :cardgame)

(defmacro set-in-front (L X)
  `(setf ,L (cons ,X ,L)))

(defun set-front-if-not (L X &key (test #'eql))
  (unless (position X L :test test)
    (set-in-front L X))
  L)
  
(defun compose (f g)
  #'(lambda (&rest x) (funcall f (apply g x))))
