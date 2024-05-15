;;;; algorithms/minimum-spanning-tree.lisp

#|
I originally wrote this algorithm to solve a Project Euler puzzle.
There are other algorithms such as the Kruskal or Prims algorithms
I didn't know about these, and this one works fine
|#

(in-package :com.danielkeogh.graph.algorithms)

(defun get-minimal-edges (graph edge-cost-fn)
  (remove-duplicates
   (utils:with-collector (collect)
     (graph:for-vertices
      graph
      (lambda (vertex)
        (let ((min-edge
                (utils:with-minimizer (minimize edge-cost-fn)
                  (graph:for-in-out-edges graph vertex #'minimize))))
          (collect min-edge)))))))

(defstruct (r-edge (:include edge:edge))
  (cost nil)
  (wrapped nil :type edge:edge))

(defun minimum-spanning-tree (graph edge-cost-fn)
  "Find the set of edges in the graph that make up a minimum spanning tree (MST)."
  (let* ((equality-fn (graph:graph-vertex-equality-fn graph))
        (minimal-edges (get-minimal-edges graph edge-cost-fn))
        (new-graph (graph:make-bidirectional-graph :vertex-equality-fn equality-fn))
        (visited-vertices (make-hash-table :test equality-fn))
        (groups (list)))
    (graph:for-vertices graph (lambda (v) (graph:add-vertex new-graph v)))
    (loop for edge in minimal-edges do (graph:add-edge new-graph edge))
    
    (graph:for-vertices
     new-graph
     (lambda (vertex)
       (unless (gethash vertex visited-vertices)
         (let ((new-group
                 (utils:with-collector (collect)
                   (bidirectional-breadth-first-search
                    new-graph
                    :root-vertex vertex
                    :on-vertex-finished-fn (lambda (v)
                                             (collect v)
                                             (setf (gethash v visited-vertices) t))))))
           (push new-group groups)))))

    (if (> (length groups) 1)
        (let ((condensated-graph (graph:make-bidirectional-graph))
              (vertex-group (make-hash-table :test equality-fn)))
          (dolist (group groups)
            (graph:add-vertex condensated-graph group)
            (dolist (vertex group)
              (setf (gethash vertex vertex-group) group)))

          (graph:for-edges
           graph
           (lambda (edge)
             (let ((source-group (gethash (graph:edge-source edge) vertex-group))
                   (target-group (gethash (graph:edge-target edge) vertex-group)))
               (unless (eql source-group target-group)
                 (graph:add-edge
                  condensated-graph
                  (make-r-edge :source source-group
                               :target target-group
                               :cost (funcall edge-cost-fn edge)
                               :wrapped edge))))))
          (let ((more-edges (minimum-spanning-tree condensated-graph #'r-edge-cost)))
            (nconc minimal-edges (mapcar #'r-edge-wrapped more-edges))))
        minimal-edges)))
