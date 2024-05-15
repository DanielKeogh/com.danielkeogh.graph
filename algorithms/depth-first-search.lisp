;;;; algorithms/depth-first-search.lisp

(in-package #:com.danielkeogh.graph.algorithms)

(defstruct (search-frame (:conc-name sf-)
                         (:type vector)
                         (:constructor make-search-frame (vertex edges depth)))
  (vertex (utils:required-argument "vertex") :read-only t)
  (edges (utils:required-argument "edges") :read-only t)
  (depth (utils:required-argument "depth") :read-only t))

(defun depth-first-search (graph root-vertex
                           &key
                             (max-depth most-positive-fixnum)
                             (on-discover-vertex-fn #'identity)
                             (on-back-edge-fn #'identity)
                             (on-forward-or-cross-edge-fn #'identity)
                             (on-vertex-finished-fn #'identity))
  (labels ((out-edges (vertex)
             (graph:out-edges graph vertex)))
    (let ((todo-stack (list (make-search-frame root-vertex (out-edges root-vertex) 0)))
          (vertex-colors (make-hash-table :test (graph:graph-vertex-equality-fn graph))))
      (setf (gethash root-vertex vertex-colors) :gray)
      (funcall on-discover-vertex-fn root-vertex)
      (loop named search
            while todo-stack do
              (let* ((frame (pop todo-stack))
                     (depth (sf-depth frame))
                     (u (sf-vertex frame))
                     (edges (sf-edges frame)))
                (when (> depth max-depth)
                  (setf (gethash u vertex-colors) :black)
                  (continue))

                (loop while edges do
                  (let* ((edge (first edges))
                         (vertex (graph:edge-target edge))
                         (color (or (gethash vertex vertex-colors) :white)))
                    (setf edges (cdr edges))
                    (case color
                      (:white
                       (push (make-search-frame u edges depth) todo-stack)
                       (setf u vertex
                             edges (out-edges u)
                             (gethash u vertex-colors) :gray)
                       (incf depth)
                       (funcall on-discover-vertex-fn vertex))

                      (:gray
                       (funcall on-back-edge-fn vertex))

                      (:black
                       (funcall on-forward-or-cross-edge-fn vertex)))))

                (funcall on-vertex-finished-fn u))))))
