;;;; algorithms/breadth-first-search.lisp

(in-package :com.danielkeogh.graph.algorithms)

(declaim (ftype (function (t &key
                             (:root-vertex t)
                             (:queue-size fixnum)
                             (:on-discover-vertex-fn (function (t)))
                             (:on-examine-vertex-fn (function (t)))
                             (:on-vertex-finished-fn (function (t)))
                             (:on-gray-target-fn (function (edge:edge)))
                             (:on-black-target-fn (function (edge:edge))))
                          (values null &optional))
                breadth-first-search))
(defun breadth-first-search (graph
                             &key
                               root-vertex
                               (queue-size 1024)
                               (on-discover-vertex-fn #'identity)
                               (on-examine-vertex-fn #'identity)
                               (on-vertex-finished-fn #'identity)
                               (on-gray-target-fn #'identity)
                               (on-black-target-fn #'identity))
  (declare #.utils:*internal-optimize-settings*)
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
          (enqueue-root root-vertex)
          (graph:for-roots graph #'enqueue-root))

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
