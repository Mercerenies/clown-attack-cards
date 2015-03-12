(in-package :cardgame)

(defmacro with-gui-log-handler (&body body)
  (let ((X (gensym)))
    `(handler-bind ((log-info #'(lambda (,X) (invoke-restart 'log-text ,X))))
                   (restart-bind ((log-text #'call-gui-log))
                                 ,@body))))

(defun call-gui-log (X)
  (java:jstatic "writeEventLog" "Main" (log-text X))
  (when (typep X 'log-info-with-flag)
    (case (log-flag X)
      (attack (java:jstatic "doClownAttack" "Main"))
      (otherwise (warn "Unrecognized log info flag '%A'!" (log-flag X))))))

(defun call-with-handler (F &rest args)
  (with-gui-log-handler (apply F args)))
