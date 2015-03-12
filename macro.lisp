(in-package :cardgame)

(defmacro define-access (F A)
  (let ((X (gensym))
        (Y (gensym)))
    `(progn
       (defun ,F (,X)
         (,A ,X))
       (defsetf ,F (,X) (,Y)
         `(setf (,',A ,,X) ,,Y)))))

(defmacro defcard (class name desc &key
                         (image nil)
                         (symbol (gensym))
                         (effect nil)
                         (triple nil)
                         (stacks t)
                         (class-options nil)
                         (class-parents nil)
                         (class-initargs nil))
  `(progn
     (defclass ,class (card ,@class-parents)
       ,class-options
       (:default-initargs :name ,name
                          :desc (format nil ,desc)
                          :stacks ,stacks
                          ,@class-initargs))
     (defmethod card-image ((,symbol ,class))
       ,image)
     (defmethod card-effect ((,symbol ,class))
       ,effect)
     (defmethod card-triple ((,symbol ,class))
       ,triple)))
