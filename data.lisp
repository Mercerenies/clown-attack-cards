(in-package :cardgame)

(defparameter *highscore*
  0)

(defun update-highscore ()
  (setq *highscore* (max *highscore* (time-days *time*))))

(defun save-highscore ()
  (with-open-file (stream "savegame.dat" :direction :output)
    (format stream "~S" *highscore*)))

(defun load-highscore ()
  (with-open-file (stream "savegame.dat" :if-does-not-exist nil)
    (when stream
      (setq *highscore* (read stream)))))
