;;;; algorithms/undirected-depth-first-search.lisp

(in-package :com.danielkeogh.graph.algorithms)

(defun no-op (&rest args)
  (declare (ignore args)))

(defun undirected-depth-first-search (undirected-graph &key
                                                         (root-vertex nil)
                                                         (process-all-vertices nil)
                                                         (max-depth most-positive-fixnum)
                                                         (on-vertex-finished-fn #'identity)
                                                         (on-vertex-max-depth-reached-fn #'identity)
                                                         (on-start-vertex-fn #'identity)
                                                         (on-discover-vertex-fn #'identity)
                                                         (on-examine-edge-fn #'no-op)
                                                         (on-tree-edge-fn #'no-op)
                                                         (on-back-edge-fn #'no-op)
                                                         (on-forward-or-cross-edge-fn #'no-op))
  (let ((visited-edges (make-hash-table))
        (vertex-colors (make-hash-table :test (graph:graph-vertex-equality-fn undirected-graph))))
    (labels ((make-frame (vertex edges depth) (vector vertex edges depth))
             (vertex (frame) (aref frame 0))
             (edges (frame) (aref frame 1))
             (depth (frame) (aref frame 2))
             (adjacent (vertex) (graph:adjacent-edges undirected-graph vertex))
             (dfs (start-vertex)
               (setf (gethash start-vertex vertex-colors) :gray)
               (funcall on-discover-vertex-fn start-vertex)
               (loop with stack = (list (make-frame start-vertex (adjacent start-vertex) 0))
                     while stack
                     for frame = (pop stack)
                     for u = (vertex frame)
                     for depth = (depth frame)
                     for edges = (edges frame)
                     do
                        (when (>= depth max-depth)
                          (funcall on-vertex-max-depth-reached-fn depth)
                          (setf (gethash u vertex-colors) :black)
                          (funcall on-vertex-finished-fn u)
                          (continue))
                        (loop while edges do 
                          (let* ((edge (pop edges)))
                            (if (gethash edge visited-edges)
                                (continue)
                                (setf (gethash edge visited-edges) t))
                            (let* ((reversed (graph:vertex-equals undirected-graph u
                                                                  (edge:edge-target edge)))
                                   (v (if reversed (edge:edge-source edge)
                                          (edge:edge-target edge))))
                              
                              (funcall on-examine-edge-fn edge reversed)
                              (case (gethash v vertex-colors :white)
                                (:white
                                 (funcall on-tree-edge-fn edge reversed)
                                 (push (make-frame u edges depth) stack)
                                 (setf u v)
                                 (incf depth)
                                 (setf edges (adjacent u))
                                 (setf (gethash u vertex-colors) :gray)
                                 (funcall on-discover-vertex-fn u))
                                (:gray
                                 (funcall on-back-edge-fn edge reversed))
                                (:black
                                 (funcall on-forward-or-cross-edge-fn edge reversed))))))
                        (setf (gethash u vertex-colors) :black)
                        (funcall on-vertex-finished-fn u)))

             (visit-all-vertices ()
               (loop for vertex in (graph:vertices undirected-graph)
                     unless (gethash vertex vertex-colors)
                       do (funcall on-start-vertex-fn vertex)
                          (dfs vertex))))
      
      (if root-vertex
          (progn (funcall on-start-vertex-fn root-vertex)
                 (dfs root-vertex)
                 (when process-all-vertices
                   (visit-all-vertices)))
          (visit-all-vertices)))))

(trivial-indent:define-indentation undirected-depth-first-search (4 &rest 1))
