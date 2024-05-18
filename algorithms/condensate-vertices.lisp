;;;; algorithms/condensate-vertices.lisp

(in-package :com.danielkeogh.graph.algorithms)

(defstruct (condensed-edge (:include edge:edge))
  (edges nil))

(defun condensate-vertices (graph get-components-fn)
  (let* ((new-graph (graph:make-bidirectional-graph)))
    (multiple-value-bind (components component-count)
        (funcall get-components-fn graph)
      (let ((condensed-vertices (make-hash-table))
            (condensed-edges (make-hash-table :test 'equal)))
        (dotimes (i component-count)
          (let ((vertex (graph:make-bidirectional-graph)))
            (setf (gethash i condensed-vertices) vertex)
            (graph:add-vertex new-graph vertex)))

        (graph:for-vertices graph
            (lambda (vertex)
              (graph:add-vertex (gethash (gethash vertex components) condensed-vertices) vertex)))

        (graph:for-edges graph
            (lambda (edge)
              (let* ((source (graph:edge-source edge))
                     (target (graph:edge-target edge))
                     (source-id (gethash source components))
                     (target-id (gethash target components))
                     (sources (gethash source-id condensed-vertices)))

                (if (= source-id target-id)
                    (graph:add-edge sources edge)
                    (let* ((edge-key (list source-id target-id))
                           (condensed-edge (gethash edge-key condensed-edges)))
                      (unless condensed-edge
                        (let ((targets (gethash target-id condensed-vertices)))
                          (setf condensed-edge (make-condensed-edge :source sources :target targets)))
                        (setf (gethash edge-key condensed-edges) condensed-edge)
                        (graph:add-edge new-graph condensed-edge))

                      (push edge (condensed-edge-edges condensed-edge)))))))))
    
    new-graph))

(defun condensate-strongly-connected-vertices (graph)
  (condensate-vertices graph #'strongly-connected-components))
