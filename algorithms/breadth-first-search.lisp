;;;; algorithms/breadth-first-search.lisp

(in-package :com.danielkeogh.graph.algorithms)

(defun breadth-first-search (graph
                             &key
                               root-vertex
                               (queue-size 1024)
                               (on-discover-vertex-fn #'identity)
                               (on-examine-vertex-fn #'identity)
                               (on-vertex-finished-fn #'identity)
                               (on-gray-target-fn #'identity)
                               (on-black-target-fn #'identity))
  (let ((queue (cl-speedy-queue:make-queue queue-size))
        (colors (make-hash-table :test (graph:graph-vertex-equality-fn graph))))
    (labels ((out-edges (vertex) (graph:out-edges graph vertex))
             (enqueue (vertex)
               (funcall on-discover-vertex-fn vertex)
               (cl-speedy-queue:enqueue vertex queue))
             (enqueue-root (root)
               (enqueue root)
               (setf (gethash root colors) :gray))
             (has-queue () (not (cl-speedy-queue:queue-empty-p queue)))
             (dequeue () (cl-speedy-queue:dequeue queue)))

      (if root-vertex 
          (enqueue root-vertex)
          (graph:for-roots graph #'enqueue))

      (loop while (has-queue) do
        (let ((vertex (dequeue)))
          (funcall on-examine-vertex-fn vertex)
          (loop for edge in (out-edges vertex) do
            (let* ((target (graph:edge-target edge))
                   (color (or (gethash target colors) :white)))
              (setf (gethash target colors) :gray)
              (case color 
                (:white
                 (setf (gethash target colors) :gray)
                 (enqueue target))
                (:gray
                 (funcall on-gray-target-fn edge))
                (:black
                 (funcall on-black-target-fn edge)))))
          (setf (gethash vertex colors) :black)
          (funcall on-vertex-finished-fn vertex))))))
