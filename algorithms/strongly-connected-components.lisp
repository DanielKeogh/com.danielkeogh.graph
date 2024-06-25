;;;; algorithms/strongly-connected-components.lisp

(in-package :com.danielkeogh.graph.algorithms)

(declaim (ftype (function (t) (values hash-table fixnum &optional))
                strongly-connected-components))
(defun strongly-connected-components (graph)
  (declare #.utils:*internal-optimize-settings*)
  (let* ((dfs-time 0)
         (component-count 0)
         (steps 0)
         (vertex-eq-fn (graph:graph-vertex-equality-fn graph))
         (roots (make-hash-table :test vertex-eq-fn :size (graph:vertex-count graph)))
         (discover-times (make-hash-table :test vertex-eq-fn :size (graph:vertex-count graph)))
         (components (make-hash-table :test vertex-eq-fn :size (graph:vertex-count graph)))
         (components-per-step (list))
         (vertices-per-step (list))
         (stack (list)))
    (declare (type (function (t t) boolean) vertex-eq-fn))
    (declare (type fixnum steps dfs-time component-count))
    (labels ((vertex-eq (vertex1 vertex2) (funcall vertex-eq-fn vertex1 vertex2))
             (min-discover-time (vertex1 vertex2)
               (if (< (the fixnum (gethash vertex1 discover-times))
                      (the fixnum (gethash vertex2 discover-times)))
                   vertex1
                   vertex2))
             (on-discover-vertex (vertex)
               (incf steps)
               (incf dfs-time)
               (setf (gethash vertex roots) vertex
                     (gethash vertex components) most-positive-fixnum
                     (gethash vertex discover-times) dfs-time)
               (push component-count components-per-step)
               (push vertex vertices-per-step)
               (push vertex stack))

             (on-vertex-finished (vertex)
               (loop for edge in (graph:out-edges graph vertex)
                     for target = (graph:edge-target edge)
                     when (= most-positive-fixnum (the fixnum (gethash target components)))
                       do (setf (gethash vertex roots) (min-discover-time (gethash vertex roots)
                                                                          (gethash target roots))))

               (when (vertex-eq (gethash vertex roots) vertex)
                 (loop for w = (pop stack)
                       do (progn
                            (setf (gethash w components) component-count)
                            (push component-count components-per-step)
                            (push w vertices-per-step)
                            (incf steps))
                       while (not (vertex-eq w vertex)))
                 (incf component-count))))
      (depth-first-search graph
       :on-discover-vertex-fn #'on-discover-vertex
       :on-vertex-finished-fn #'on-vertex-finished))
    (values components component-count)))
