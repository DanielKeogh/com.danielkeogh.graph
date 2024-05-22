;;;; algorithms/weakly-connected-components.lisp

(in-package :com.danielkeogh.graph.algorithms)

(defun weakly-connected-components (graph)
  (let ((current-component 0)
        (component-count 0)
        (component-equivalences (make-hash-table))
        (components (make-hash-table)))
    (labels ((on-start-vertex (vertex)
               (setf current-component (hash-table-count component-equivalences)
                     (gethash current-component component-equivalences) current-component
                     (gethash vertex components) current-component)
               (incf component-count))

             (on-edge-discovered (edge)
               (setf (gethash (graph:edge-target edge) components) current-component))

             (on-forward-or-cross-edge (edge)
               (let* ((target (graph:edge-target edge))
                      (other-component (get-component-equivalence (gethash target components))))
                 (when (/= current-component other-component)
                   (decf component-count)
                   (if (> current-component other-component)
                       (progn
                         (setf (gethash current-component component-equivalences) other-component
                               current-component other-component))
                       (progn
                         (setf (gethash other-component component-equivalences) current-component))))))
             (get-component-equivalence (component)
               (let ((equivalent component)
                     (compress nil))
                 (loop for temp = (gethash equivalent component-equivalences)
                       while (/= temp equivalent) do
                         (setf equivalent temp
                               compress t))

                 (when compress
                   (loop for temp = (gethash component component-equivalences)
                         while (/= temp equivalent) do
                           (setf (gethash component component-equivalences) equivalent)))
                 equivalent)))
      
      (depth-first-search graph
       :on-start-vertex-fn #'on-start-vertex
       :on-tree-edge-fn #'on-edge-discovered
       :on-forward-or-cross-edge-fn #'on-forward-or-cross-edge)
      (values components component-count))))
