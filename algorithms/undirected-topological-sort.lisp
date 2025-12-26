;;;; algorithms/undirected-topological-sort.lisp

(in-package :com.danielkeogh.graph.algorithms)

(defun undirected-topological-sort (undirected-graph &key (allow-cyclic-graph nil))
  (let ((sorted-vertices (make-array (graph:vertex-count undirected-graph) :fill-pointer 0)))
    (labels ((on-back-edge (edge reversed)
               (declare (ignore edge reversed))
               (unless allow-cyclic-graph
                 (error "Cannot do topological sort on graph including cycles.")))
             (on-vertex-finished (vertex)
               (vector-push vertex sorted-vertices)))
      (undirected-depth-first-search undirected-graph
       :on-back-edge-fn #'on-back-edge
       :on-vertex-finished-fn #'on-vertex-finished))
    (nreverse sorted-vertices)))
