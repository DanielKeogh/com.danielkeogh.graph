;;;; algorithms/depth-first-search.lisp

(in-package #:com.danielkeogh.graph.algorithms)

(defstruct (search-frame (:conc-name sf-)
                         (:type vector)
                         (:constructor make-search-frame (vertex edges depth)))
  (vertex (utils:required-argument "vertex") :read-only t)
  (edges (utils:required-argument "edges") :read-only t)
  (depth (utils:required-argument "depth") :read-only t))

(defun depth-first-search (graph
                           &key
                             (root-vertex nil)
                             (process-all-vertices nil)
                             (max-depth most-positive-fixnum)
                             (on-start-vertex-fn #'identity)
                             (on-tree-edge-fn #'identity)
                             (on-discover-vertex-fn #'identity)
                             (on-back-edge-fn #'identity)
                             (on-forward-or-cross-edge-fn #'identity)
                             (on-vertex-finished-fn #'identity))
  (declare #.utils:*internal-optimize-settings*)
  (let ((vertex-colors (make-hash-table :test (graph:graph-vertex-equality-fn graph))))
    (labels ((out-edges (vertex)
               (graph:out-edges graph vertex))

             (dfs (start-vertex)
               (let ((todo-stack (list (make-search-frame start-vertex (out-edges start-vertex) 0))))
                 (setf (gethash start-vertex vertex-colors) :gray)
                 (funcall on-discover-vertex-fn start-vertex)
                 (loop named search
                       while todo-stack do
                         (let* ((frame (pop todo-stack))
                                (depth (sf-depth frame))
                                (u (sf-vertex frame))
                                (edges (sf-edges frame)))
                           (declare (type fixnum depth))
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
                                  (funcall on-tree-edge-fn edge)
                                  (push (make-search-frame u edges depth) todo-stack)
                                  (setf u vertex
                                        edges (out-edges u)
                                        (gethash u vertex-colors) :gray)
                                  (incf depth)
                                  (funcall on-discover-vertex-fn vertex))

                                 (:gray
                                  (funcall on-back-edge-fn edge))

                                 (:black
                                  (funcall on-forward-or-cross-edge-fn edge)))))

                           (funcall on-vertex-finished-fn u)))))
             (visit-all-vertices ()
               (loop for vertex in (graph:vertices graph)
                     unless (gethash vertex vertex-colors)
                       do (funcall on-start-vertex-fn vertex)
                          (dfs vertex))))
      (if root-vertex
          (progn (dfs root-vertex)
                 (funcall on-start-vertex-fn root-vertex)
                 (when process-all-vertices
                   (visit-all-vertices)))
          (visit-all-vertices)))))

(trivial-indent:define-indentation depth-first-search (4 &rest 1))
