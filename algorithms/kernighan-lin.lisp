;;;; algorithms/kernighan-lin.lisp

(in-package :com.danielkeogh.graph.algorithms)

(declaim (ftype (function (t fixnum (function (edge:edge) (values number &optional)))
                          (values hash-table hash-table number &optional))
                kernighan-lin-partition))
(defun kernighan-lin-partition (graph iterations edge-cost-fn)
  (labels ((make-set () (make-hash-table :test (graph:graph-vertex-equality-fn graph))))
    (let ((vertex-set-a (make-set))
          (vertex-set-b (make-set))
          (partition-size (floor (the fixnum (graph:vertex-count graph)) 2)))
      ;; Initialize start partitions
      (let ((i 0))
        (declare (type fixnum i))
        (graph:for-vertices graph
            (lambda (v)
              (if (< i partition-size)
                  (setf (gethash v vertex-set-a) t)
                  (setf (gethash v vertex-set-b) t))
              (incf i))))

      (let ((unswapped-set-a (alexandria:copy-hash-table vertex-set-a
                                                         :test (graph:graph-vertex-equality-fn graph)))
            (unswapped-set-b (alexandria:copy-hash-table vertex-set-b
                                                         :test (graph:graph-vertex-equality-fn graph))))

        (labels ((cost (edge) (the number (funcall edge-cost-fn edge)))
                 (cut-cost (arr) (aref arr 2))
                 (find-edge (v1 v2)
                   (loop for edge in (graph:adjacent-edges graph v1)
                         when (or (graph:vertex-equals graph (graph:edge-source edge) v2)
                                  (graph:vertex-equals graph (graph:edge-target edge) v2))
                           do (return edge)))
                 (swap-vertices (set-a vertex-a set-b vertex-b)
                   (remhash vertex-a set-a)
                   (remhash vertex-b set-b)
                   (setf (gethash vertex-a set-b) t)
                   (setf (gethash vertex-b set-a) t))
                 (get-cut-cost ()
                   (let ((cost 0))
                     (declare (type number cost))
                     (graph:for-edges graph
                         (lambda (edge)
                           (edge:with-edge (source target) edge
                             (unless (eq (gethash source vertex-set-a) (gethash target vertex-set-a))
                               (incf cost (cost edge))))))
                     cost))
                 (get-vertex-cost (vertex)
                   (let ((cost 0)
                         (vertex-is-in-a (gethash vertex vertex-set-a)))
                     (declare (type number cost))
                     (dolist (edge (graph:adjacent-edges graph vertex))
                       (edge:with-edge (source target) edge
                         (let ((neighbor (if (eq source vertex) target source)))
                           (if (eq vertex-is-in-a (gethash neighbor vertex-set-a))
                               (incf cost (cost edge))
                               (decf cost (cost edge))))))
                     cost))
                 (do-all-swaps ()
                   (let ((swaps (list))
                         (min-cost most-positive-double-float)
                         (min-id -1))
                     (declare (type number min-cost))
                     (labels ((single-swap ()
                                (let ((max-gain most-negative-double-float)
                                      max-a max-b)
                                  (loop for vertex-a being the hash-keys of unswapped-set-a do
                                    (loop for vertex-b being the hash-keys of unswapped-set-b do
                                      (let* ((edge (find-edge vertex-a vertex-b))
                                             (edge-cost (if edge (cost edge) 0))
                                             (gain (* edge-cost
                                                      (+ (get-vertex-cost vertex-a)
                                                         (get-vertex-cost vertex-b)
                                                         -1))))
                                        (when (> gain max-gain)
                                          (setf max-a vertex-a 
                                                max-b vertex-b
                                                max-gain gain)))))
                                  (unless (or max-a max-b)
                                    (error "Must find a swap"))
                                  (swap-vertices vertex-set-a max-a vertex-set-b max-b)
                                  (push (cons max-a max-b) swaps)
                                  (remhash max-a unswapped-set-a)
                                  (remhash max-b unswapped-set-b))

                                (get-cut-cost)))

                       
                       (dotimes (i partition-size)
                         (let ((cost (single-swap)))
                           (declare (type number cost))
                           (when (< cost min-cost)
                             (setf min-cost cost)
                             (setf min-id i)))))
                     (loop while (> (1- (length swaps)) min-id)
                           for (vertex1 . vertex2) = (pop swaps)
                           do (swap-vertices vertex-set-a vertex2 vertex-set-b vertex1))
                     
                     (vector vertex-set-a vertex-set-b min-cost))))
          
          (let ((best-partition (vector vertex-set-a vertex-set-b 0))
                (min-cost most-positive-double-float))

            (loop repeat iterations
                  for tmp-partition = (do-all-swaps)
                  for tmp-cost = (cut-cost tmp-partition)
                  do
                     (setf vertex-set-a (aref tmp-partition 0)
                           vertex-set-b (aref tmp-partition 1)
                           unswapped-set-a vertex-set-a
                           unswapped-set-b vertex-set-b)
                     (when (< tmp-cost min-cost)
                       (setf best-partition tmp-partition
                             min-cost tmp-cost)))
            (values (aref best-partition 0)
                    (aref best-partition 1)
                    (aref best-partition 2))))))))
