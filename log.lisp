(in-package :cardgame)

(define-condition log-info (simple-condition)
  ((text :accessor log-text
         :initform ""
         :initarg :text
         :type string)))

(define-condition log-info-with-flag (log-info)
  ((flag :accessor log-flag
         :initform nil
         :initarg :flag
         :type symbol)))

(defmacro with-log-handler (&body body)
  (let ((X (gensym)))
    `(handler-bind ((log-info #'(lambda (,X) (invoke-restart 'log-text (log-text ,X)))))
                   (restart-bind ((log-text #'(lambda (,X) (format t "~%~A~%" ,X))))
                                 ,@body))))

(defun write-log (text &optional (flag nil to-flag))
  (if to-flag
      (signal 'log-info-with-flag :text text :flag flag)
      (signal 'log-info :text text)))

(defun format-log (text &rest format)
  (write-log (apply #'format nil text format)))
