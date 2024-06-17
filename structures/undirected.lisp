;;;; undirected.lisp

(defpackage #:com.danielkeogh.graph.undirected
  (:use #:cl)
  (:local-nicknames
   (#:utils #:com.danielkeogh.graph.utils)
   (#:edge #:com.danielkeogh.graph.edge)
   (#:a #:alexandria))
  (:export
   ;; constructors
   #:make-graph

   ;; builders
   #:add-vertex
   #:remove-vertex
   #:add-edge
   #:add-edge-between
   #:remove-edge
   #:remove-edge-between

   ;; test
   #:has-vertex
   #:has-edge
   #:has-edge-between

   ;; utils
   #:vertices
   #:edges
   #:adjacent-edges
   #:vertex-count
   #:edge-count

   ;; accessors
   #:graph-vertex-equality-fn

   ;; looping without malloc
   #:for-edges
   #:for-adjacent-eges
   #:for-vertices

   ;; types
   #:undirected-graph
   #:undirected-graph-p))

(in-package #:com.danielkeogh.graph.undirected)

;;; struct definitions

(defstruct (undirected-graph (:conc-name graph-))
  (allow-parallel-edges (utils:required-argument "allow-parallel-edges") :type boolean :read-only t)
  (vertex-edges (utils:required-argument "vertex-edges") :type hash-table :read-only t)
  (vertex-equality-fn (utils:required-argument "vertex-equality-fn") :type function :read-only t))


;;; constructors

(defun make-graph (&key
                     (allow-parallel-edges t)
                     (vertex-equality-fn #'eql))
  (declare #.utils:*internal-optimize-settings*)
  (make-undirected-graph
   :allow-parallel-edges allow-parallel-edges
   :vertex-edges (make-hash-table :test vertex-equality-fn)
   :vertex-equality-fn vertex-equality-fn))

(defun make-edge-list ()
  (list))

;; macros

(defmacro ensure-vertex (graph vertex)
  `(unless (has-vertex ,graph ,vertex)
     (error "Unrecognized ~S vertex ~S. Call add-vertex first." (prin1-to-string ',vertex) ,vertex)))

;; utils

(defun edge-equal (graph edge source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (let ((test (graph-vertex-equality-fn graph)))
    (or (and (funcall test source (edge:edge-source edge))
             (funcall test target (edge:edge-target edge)))
        (and (funcall test target (edge:edge-source edge))
             (funcall test source (edge:edge-target edge))))))

;; api

(defun has-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (when (nth-value 1 (gethash vertex (graph-vertex-edges graph)))
    t)) ;; return boolean type for speed and to protect against memory leaks

(defun has-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (let ((edges (gethash (edge:edge-source edge) (graph-vertex-edges graph))))
    (declare (type (or list null) edges))
    (when edges
        (find edge edges :test #'eql))))

(defun has-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (let ((edges (gethash source (graph-vertex-edges graph))))
    (declare (type (or list null) edges))
    (when edges
      (loop for edge in edges
              thereis (edge-equal graph edge source target)))))

(defun add-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (unless (has-vertex graph vertex)
    (setf (gethash vertex (graph-vertex-edges graph)) (make-edge-list))))

(defun %add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (labels ((add-edge-to-hash (hashtable vertex edge)
             (push edge (gethash vertex hashtable))))
    (let ((source (edge:edge-source edge))
          (target (edge:edge-target edge)))
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (when (or (graph-allow-parallel-edges graph)
                (not (has-edge-between graph source target)))
        (add-edge-to-hash (graph-vertex-edges graph) source edge)
        (unless (eql source target)
          (add-edge-to-hash (graph-vertex-edges graph) target edge))
        edge))))

(defun add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (%add-edge graph edge))

(defun add-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (%add-edge graph (edge:make-edge source target)))

(defun remove-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (let ((edgemap (graph-vertex-edges graph)))
    (edge:with-edge (source target) edge
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (setf (gethash source edgemap) (remove edge (gethash source edgemap))
            (gethash target edgemap) (remove edge (gethash target edgemap))))))

(defun remove-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (ensure-vertex graph source)
  (ensure-vertex graph target)
  (let ((edgemap (graph-vertex-edges graph))
        (should-remove-edge (lambda (edge) (edge-equal graph edge source target))))
    (setf (gethash source edgemap) (remove-if should-remove-edge (gethash source edgemap))
          (gethash target edgemap) (remove-if should-remove-edge (gethash target edgemap)))))

(defun remove-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (let* ((edgemap (graph-vertex-edges graph))
         (edges (gethash vertex edgemap)))
    (remhash vertex edgemap)
    (dolist (edge edges)
      (edge:with-edge (source target) edge
        (if (gethash source edgemap)
            (setf (gethash source edges) (remove edge (gethash source edges)))
            (setf (gethash target edges) (remove edge (gethash target edges))))))))

;;; looping without malloc

(defun for-edges (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type function fn))
  (maphash (lambda (vertex edges)
             (dolist (edge edges)
               (when (eq (edge:edge-source edge) vertex)
                 (funcall fn edge))))
           (graph-vertex-edges graph)))

(defun for-vertices (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type function fn))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        do (funcall fn vertex)))

(defun for-adjacent-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type function fn))
  (loop for edge in (adjacent-edges graph vertex)
        do (funcall fn edge)))

;;; utils

(defun edges (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (utils:with-collector (collect)
    (for-edges graph #'collect)))

(defun vertices (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        collect vertex))

(defun adjacent-edges (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (gethash vertex (graph-vertex-edges graph)))

(defun vertex-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (hash-table-count (graph-vertex-edges graph)))

(defun edge-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (utils:with-counter (cnt)
    (for-edges graph #'cnt)))

