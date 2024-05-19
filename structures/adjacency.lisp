;;;; structures/adjacency.lisp

(defpackage #:com.danielkeogh.graph.adjacency
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
   #:in-edges
   #:out-edges
   #:vertex-count
   #:edge-count

   ;; accessors
   #:graph-vertex-equality-fn

   ;; looping without malloc
   #:for-edges
   #:for-vertices
   #:for-out-edges

   ;; types
   #:adjacency-graph
   #:adjacency-graph-p))

(in-package #:com.danielkeogh.graph.adjacency)

;;; struct definitions

(defstruct (adjacency-graph (:conc-name graph-))
  (allow-parallel-edges (utils:required-argument "allow-parallel-edges") :type boolean :read-only t)
  (vertex-edges (utils:required-argument "vertex-edges") :type hash-table :read-only t)
  (vertex-equality-fn (utils:required-argument "vertex-equality-fn") :type function :read-only t))

;;; constructors

(defun make-graph (&key
                     (allow-parallel-edges t)
                     (vertex-equality-fn #'eql))
  (make-adjacency-graph
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
  (let ((test (graph-vertex-equality-fn graph)))
    (and (funcall test source (edge:edge-source edge))
         (funcall test target (edge:edge-target edge)))))

;; api

(defun has-vertex (graph vertex)
  (declare (type adjacency-graph graph))
  (when (nth-value 1 (gethash vertex (graph-vertex-edges graph)))
    t))

(defun has-edge (graph edge)
  (declare (type adjacency-graph graph))
  (a:when-let (edges (gethash (edge:edge-source edge) (graph-vertex-edges graph)))
    (find edge edges :test #'eql)))

(defun has-edge-between (graph source target)
  (declare (type adjacency-graph graph))
  (a:when-let (edges (gethash source (graph-vertex-edges graph)))
    (loop for edge in edges
            thereis (edge-equal graph edge source target))))

(defun add-vertex (graph vertex)
  (declare (type adjacency-graph graph))
  (unless (has-vertex graph vertex)
    (setf (gethash vertex (graph-vertex-edges graph)) (make-edge-list))))

(defun %add-edge (graph edge)
  (declare (type adjacency-graph graph)
           (type edge:edge edge)
           (optimize (speed 3) (safety 0)))
  (let ((source (edge:edge-source edge))
        (target (edge:edge-target edge)))
    (ensure-vertex graph source)
    (ensure-vertex graph target)
    (when (or (graph-allow-parallel-edges graph)
              (not (has-edge-between graph source target)))
      (push edge (gethash source (graph-vertex-edges graph)))
      edge)))

(defun add-edge (graph edge)
  (declare (type adjacency-graph graph)
           (type edge:edge edge))
  (%add-edge graph edge))

(defun add-edge-between (graph source target)
  (declare (type adjacency-graph graph))
  (%add-edge graph (edge:make-edge source target)))

(defun remove-edge (graph edge)
  (declare (type adjacency-graph graph)
           (type edge:edge edge))
  (edge:with-edge (source target) edge
    (ensure-vertex graph source)
    (ensure-vertex graph target)
    (setf (gethash source (graph-vertex-edges graph))
          (remove edge (gethash source (graph-vertex-edges graph))))))

(defun remove-edge-between (graph source target)
  (declare (type adjacency-graph graph))
  (ensure-vertex graph source)
  (ensure-vertex graph target)
  (let ((should-remove-edge (lambda (edge) (edge-equal graph edge source target))))
    (setf (gethash source (graph-vertex-edges graph))
          (remove-if should-remove-edge (gethash source (graph-vertex-edges graph))))))

(defun remove-in-edges (graph filter-fn)
  (declare (type adjacency-graph graph)
           (type function filter-fn))
  (let ((vertex-hash (graph-vertex-edges graph)))
    (with-hash-table-iterator (itr vertex-hash)
      (loop
        (multiple-value-bind (more? key val)
            (itr)
          (unless more? (return))
          (setf (gethash key vertex-hash) (remove-if filter-fn val)))))))

(defun remove-vertex (graph vertex)
  (declare (type adjacency-graph graph))
  (let ((eq-fn (graph-vertex-equality-fn graph)))
    (remhash vertex (graph-vertex-edges graph))
    (remove-in-edges graph (lambda (edge) (funcall eq-fn vertex (edge:edge-target edge))))))

;; utils

(defun edges (graph)
  (declare (type adjacency-graph graph))
  (loop for edge-collection being the hash-values of (graph-vertex-edges graph)
        nconc (copy-list edge-collection)))

(defun vertices (graph)
  (declare (type adjacency-graph graph))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        collect vertex))

(defun out-edges (graph vertex)
  (declare (type adjacency-graph graph))
  (gethash vertex (graph-vertex-edges graph)))

(defun in-edges (graph vertex)
  (declare (type adjacency-graph graph))
  (let ((vertex-hash (graph-vertex-edges graph))
        (vertex-eq (graph-vertex-equality-fn graph)))
    (loop for val being the hash-values of vertex-hash
          nconc (remove-if-not
                 (lambda (edge)
                   (funcall vertex-eq vertex (edge:edge-target edge)))
                 val))))

(defun vertex-count (graph)
  (declare (type adjacency-graph graph))
  (hash-table-count (graph-vertex-edges graph)))

(defun edge-count (graph)
  (declare (type adjacency-graph graph))
  (loop for edge-collection being the hash-values of (graph-vertex-edges graph)
        sum (length edge-collection)))

;; looping without malloc

(defun for-edges (graph fn)
  (declare (type adjacency-graph graph)
           (type function fn))
  (loop for edge-collection being the hash-values of (graph-vertex-edges graph)
        do (loop for edge in edge-collection
                 do (funcall fn edge))))

(defun for-vertices (graph fn)
  (declare (type adjacency-graph graph)
           (type function fn))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        do (funcall fn vertex)))

(defun for-out-edges (graph vertex fn)
  (declare (type adjacency-graph graph)
           (type function fn))
  (loop for edge in (out-edges graph vertex)
        do (funcall fn edge)))
