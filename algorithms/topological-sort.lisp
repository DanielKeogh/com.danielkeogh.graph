;;;; algorithms/topological-sort.lisp

(in-package #:com.danielkeogh.graph.algorithms)

(defun topological-sort (graph)
  (let ((sorted-vertices (make-array (graph:vertex-count graph) :fill-pointer 0)))
    (labels ((on-back-edge (edge)
               (declare (ignore edge))
               (error "Cannot do topological on graph including cycles."))
             (on-vertex-finished (vertex)
               (vector-push vertex sorted-vertices)))
      (depth-first-search graph
       :on-back-edge-fn #'on-back-edge
       :on-vertex-finished-fn #'on-vertex-finished)
      (nreverse sorted-vertices))))
