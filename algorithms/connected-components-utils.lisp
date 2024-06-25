;;;; algorithms/connected-components-utils.lisp

(in-package :com.danielkeogh.graph.algorithms)

(declaim (ftype (function (t hash-table fixnum)
                          (values simple-array &optional))
                connected-components->graphs))
(defun connected-components->graphs (source-graph components component-count)
  (declare #.utils:*internal-optimize-settings*)
  (let ((graphs (make-array component-count)))
    (dotimes (i component-count)
      (setf (aref graphs i) (graph:make-bidirectional-graph)))

    (loop for vertex being the hash-keys of components
          do (graph:add-vertex (aref graphs (gethash vertex components)) vertex))

    (graph:for-vertices source-graph
        (lambda (vertex)
          (loop for edge in (graph:out-edges source-graph vertex)
                when (eq (gethash vertex components) (gethash (graph:edge-target edge) components))
                  do (graph:add-edge (aref graphs (gethash vertex components)) edge))))
    
    graphs))
