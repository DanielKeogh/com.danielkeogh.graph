;;;; utils.lisp

(defpackage #:com.danielkeogh.graph.utils
  (:use #:cl)
  (:export
   #:required-argument
   #:let-with-string-streams
   #:with-collector
   #:with-maximizer
   #:with-minimizer
   #:with-counter))

(in-package #:com.danielkeogh.graph.utils)

(defun required-argument (name)
  (alexandria:required-argument name))

(defmacro let-with-string-streams (vars with-streams &body body)
  "Creating a bunch of nested `with-output-to-string` and then binding them to separate variables is a pain.
This macro takes that pain away! You can use it like this:
```
(utils:let-with-string-streams (s1 s2)
   (progn (princ \"foo\" s1)
          (princ \"bar\" s2))
   (values s1 s2))
```
"
  `(let (,@vars)
     ,(loop for var in vars
            for result = `(setf ,var (with-output-to-string (,var)
                                       ,with-streams))
              then `(setf ,var (with-output-to-string (,var)
                                  ,result))
        finally (return result))  
     ,@body))

(defmacro with-collector ((collection-fn-name) &body body)
  "Collect a list"
  (alexandria:with-gensyms (collection current-cons item)
    `(let* ((,collection (list nil))
            (,current-cons ,collection))
       (labels ((,collection-fn-name (,item)
                  (setf (cdr ,current-cons) (cons ,item nil)
                        ,current-cons (cdr ,current-cons))))
         ,@body)
       (cdr ,collection))))

(defmacro with-maximizer ((maximiser-fn &optional (value-fn #'identity)) &body body)
  "Find the maximum"
  (alexandria:with-gensyms (max max-val arg arg-val)
    `(let (,max ,max-val)
       (labels ((,maximiser-fn (,arg)
                  (let ((,arg-val (funcall ,value-fn ,arg)))
                    (when (or (not ,max-val)
                              (> ,arg-val ,max-val))
                      (setf ,max ,arg
                            ,max-val ,arg-val)))))
         ,@body)
       (values ,max ,max-val))))

(defmacro with-minimizer ((minimizer-fn &optional (value-fn #'identity)) &body body)
  "Find the minimum"
  (alexandria:with-gensyms (max max-val arg arg-val)
    `(let (,max ,max-val)
       (labels ((,minimizer-fn (,arg)
                  (let ((,arg-val (funcall ,value-fn ,arg)))
                    (when (or (not ,max-val)
                              (< ,arg-val ,max-val))
                      (setf ,max ,arg
                            ,max-val ,arg-val)))))
         ,@body)
       (values ,max ,max-val))))

(defmacro with-counter ((counter-fn) &body body)
  "Count"
  (alexandria:with-gensyms (count arg)
    `(let ((,count 0))
       (labels ((,counter-fn (,arg)
                  (when ,arg
                    (incf ,count))))
         ,@body)
       ,count)))
