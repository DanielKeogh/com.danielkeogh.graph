;;;; hungarian-assignment.lisp

(in-package :com.danielkeogh.graph.algorithms)

(defun hungarian-assignment (costs)
  (declare (type (array number 2) costs))
  (let* ((height (array-dimension costs 0))
         (width (array-dimension costs 1))
         (masks (make-array (list height width) :element-type 'unsigned-byte))
         (rows-covered (make-array height :element-type 'boolean :initial-element nil))
         (cols-covered (make-array width :element-type 'boolean :initial-element nil))
         (path-start (cons 0 0))
         (path (make-array (list (* width height))))
         (invalid-location (cons -1 -1)))

    (labels ((make-h-location (row col) (cons row col))
             (hl-row (c) (car c))
             (hl-col (c) (cdr c))

             (clear-covers ()
               (dotimes (i height) (setf (aref rows-covered i) nil))
               (dotimes (j width) (setf (aref cols-covered j) nil)))

             (clear-primes ()
               (dotimes (i height)
                 (dotimes (j width)
                   (when (= 2 (aref masks i j))
                     (setf (aref masks i j) 0)))))
             
             (find-minimum ()
               (let ((min most-positive-fixnum))
                 (loop for i below height
                       unless (aref rows-covered i)
                         do (loop for j below width
                                  unless (aref cols-covered j)
                                    do (setf min (min (aref costs i j) min))))
                 min))
             
             (init ()
               (loop for i below height
                     for min = (loop for j below width
                                     minimize (aref costs i j))
                     do (loop for j below width
                              do (decf (aref costs i j) min)))
               (dotimes (i height)
                 (dotimes (j width)
                   (when (and (= 0 (aref costs i j))
                              (not (aref rows-covered i))
                              (not (aref cols-covered j)))
                     (setf (aref masks i j) 1
                           (aref rows-covered i) t
                           (aref cols-covered j) t))))
               (clear-covers))

             (find-zero ()
               (dotimes (i height)
                 (dotimes (j width)
                   (when (and (= 0 (aref costs i j))
                              (not (aref rows-covered i))
                              (not (aref cols-covered j)))
                     (return-from find-zero (make-h-location i j)))))
               invalid-location)

             (find-star-in-row (row)
               (dotimes (j width)
                 (when (= 1 (aref masks row j))
                   (return-from find-star-in-row j)))
               -1)

             (find-star-in-col (col)
               (dotimes (i height)
                 (when (= 1 (aref masks i col))
                   (return-from find-star-in-col i)))
               -1)

             (find-prime-in-row (row)
               (dotimes (j width)
                 (when (= 2 (aref masks row j))
                   (return-from find-prime-in-row j)))
               -1)

             (convert-path (path-length)
               (dotimes (i path-length)
                 (with-accessors ((r hl-row)
                                  (c hl-col))
                     (aref path i)
                   (case (aref masks r c)
                     (1 (setf (aref masks r c) 0))
                     (2 (setf (aref masks r c) 1))))))

             (step-1 ()
               (dotimes (i height)
                 (dotimes (j width)
                   (when (= 1 (aref masks i j))
                     (setf (aref cols-covered j) t))))
               (if (= height (loop for j below width
                                   count (aref cols-covered j)))
                   :end
                   :step2))

             (step-2 ()
               (let ((loc (find-zero)))
                 (when (= -1 (hl-row loc))
                   (return-from step-2 :step4))
                 (setf (aref masks (hl-row loc) (hl-col loc)) 2)
                 (let ((star-col (find-star-in-row (hl-row loc))))
                   (if (/= -1 star-col)
                       (progn
                         (setf (aref rows-covered (hl-row loc)) t)
                         (setf (aref cols-covered star-col) nil)
                         :step2)
                       (progn
                         (setf path-start loc)
                         :step3)))))
             (step-3 ()
               (setf (aref path 0) path-start)
               (let ((path-index 0))
                 (loop with row = (find-star-in-col (hl-col (aref path path-index)))
                       while (/= -1 row)
                       do
                          (incf path-index)
                          (setf (aref path path-index)
                                (make-h-location row (hl-col (aref path (1- path-index)))))
                          (let ((col (find-prime-in-row (hl-row (aref path path-index)))))

                            (incf path-index)
                            (setf (aref path path-index)
                                  (make-h-location (hl-row (aref path (1- path-index))) col))
                            (setf row (find-star-in-col (hl-col (aref path path-index))))))
                 (convert-path (1+ path-index))
                 (clear-covers)
                 (clear-primes)
                 :step1))

             (step-4 ()
               (let ((min-value (find-minimum)))
                 (dotimes (i height)
                   (dotimes (j width)
                     (when (aref rows-covered i)
                       (incf (aref costs i j) min-value))
                     (unless (aref cols-covered j)
                       (decf (aref costs i j) min-value))))
                 :step2)))
      (init)
      
      (loop 
        for step = :step1 then
                          (case step
                            (:step1 (step-1))
                            (:step2 (step-2))
                            (:step3 (step-3))
                            (:step4 (step-4)))
        until (eq step :end))

      (let ((result (make-array height :initial-element 0)))
        (dotimes (i height)
          (dotimes (j width)
            (when (= 1 (aref masks i j))
              (setf (aref result i) j)
              (return))))

        result))))
