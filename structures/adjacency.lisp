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

(declaim (ftype (function (&key (:allow-parallel-edges boolean)
                                (:vertex-equality-fn (function (t t) boolean)))
                          (values adjacency-graph &optional))
                make-graph))
(defun make-graph (&key
                     (allow-parallel-edges t)
                     (vertex-equality-fn #'eql))
  (declare #.utils:*internal-optimize-settings*)
  (make-adjacency-graph
   :allow-parallel-edges allow-parallel-edges
   :vertex-edges (make-hash-table :test vertex-equality-fn)
   :vertex-equality-fn vertex-equality-fn))

(declaim (ftype (function () (values list &optional))
                make-edge-list))
(defun make-edge-list ()
  (declare #.utils:*internal-optimize-settings*)
  (list))

;; macros

(defmacro ensure-vertex (graph vertex)
  `(unless (has-vertex ,graph ,vertex)
     (error "Unrecognized ~S vertex ~S. Call add-vertex first." (prin1-to-string ',vertex) ,vertex)))

;; utils

(declaim (ftype (function (adjacency-graph edge:edge t t)
                          (values boolean &optional))
                edge-equal))
(defun edge-equal (graph edge source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type edge:edge edge))
  (let ((test (graph-vertex-equality-fn graph)))
    (and (funcall test source (edge:edge-source edge))
         (funcall test target (edge:edge-target edge)))))

;; api

(declaim (ftype (function (adjacency-graph t)
                          (values boolean &optional))
                has-vertex))
(defun has-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (when (nth-value 1 (gethash vertex (graph-vertex-edges graph)))
    t))

(declaim (ftype (function (adjacency-graph edge:edge)
                          (values boolean &optional))
                has-edge))
(defun has-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type edge:edge edge))
  (let ((edges (gethash (edge:edge-source edge) (graph-vertex-edges graph))))
    (declare (type (or null list) edges))
    (when edges
      (find edge edges :test #'eql))))

(declaim (ftype (function (adjacency-graph t t)
                          (values boolean &optional))
                has-edge-between))
(defun has-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (let ((edges (gethash source (graph-vertex-edges graph))))
    (declare (type (or null list) edges))
    (when edges
      (loop for edge in edges
              thereis (edge-equal graph edge source target)))))

(declaim (ftype (function (adjacency-graph t)
                          (values boolean &optional))
                add-vertex))
(defun add-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (unless (has-vertex graph vertex)
    (setf (gethash vertex (graph-vertex-edges graph)) (make-edge-list))
    t))


(declaim (ftype (function (adjacency-graph edge:edge)
                          (values (or null edge:edge) &optional))
                %add-edge))
(defun %add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type edge:edge edge))
  (let ((source (edge:edge-source edge))
        (target (edge:edge-target edge)))
    (ensure-vertex graph source)
    (ensure-vertex graph target)
    (when (or (graph-allow-parallel-edges graph)
              (not (has-edge-between graph source target)))
      (push edge (gethash source (graph-vertex-edges graph)))
      edge)))

(declaim (ftype (function (adjacency-graph edge:edge)
                          (values (or null edge:edge) &optional))
                add-edge))
(defun add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type edge:edge edge))
  (%add-edge graph edge))

(declaim (ftype (function (adjacency-graph t t)
                          (values (or null edge:edge) &optional))
                add-between-edge))
(defun add-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (%add-edge graph (edge:make-edge source target)))

(declaim (ftype (function (adjacency-graph edge:edge)
                          (values null &optional))
                remove-edge))
(defun remove-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type edge:edge edge))
  (edge:with-edge (source target) edge
    (ensure-vertex graph source)
    (ensure-vertex graph target)
    (setf (gethash source (graph-vertex-edges graph))
          (remove edge (gethash source (graph-vertex-edges graph)))))
  nil)

(declaim (ftype (function (adjacency-graph edge:edge)
                          (values null &optional))
                remove-between-edge))
(defun remove-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (ensure-vertex graph source)
  (ensure-vertex graph target)
  (let ((should-remove-edge (lambda (edge) (edge-equal graph edge source target))))
    (setf (gethash source (graph-vertex-edges graph))
          (remove-if should-remove-edge (gethash source (graph-vertex-edges graph)))))
  nil)

(declaim (ftype (function (adjacency-graph (function (edge:edge) boolean))
                          (values null &optional))
                remove-in-edges))
(defun remove-in-edges (graph filter-fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type function filter-fn))
  (let ((vertex-hash (graph-vertex-edges graph)))
    (with-hash-table-iterator (itr vertex-hash)
      (loop
        (multiple-value-bind (more? key val)
            (itr)
          (unless more? (return))
          (setf (gethash key vertex-hash) (remove-if filter-fn val)))))))

(declaim (ftype (function (adjacency-graph t)
                          (values null &optional))
                remove-vertex))
(defun remove-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (let ((eq-fn (graph-vertex-equality-fn graph)))
    (remhash vertex (graph-vertex-edges graph))
    (remove-in-edges graph (lambda (edge) (funcall eq-fn vertex (edge:edge-target edge))))))

;; utils

(declaim (ftype (function (adjacency-graph)
                          (values list &optional))
                edges))
(defun edges (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (loop for edge-collection being the hash-values of (graph-vertex-edges graph)
        nconc (copy-list edge-collection)))

(declaim (ftype (function (adjacency-graph)
                          (values list &optional))
                vertices))
(defun vertices (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        collect vertex))

(declaim (ftype (function (adjacency-graph t)
                          (values list &optional))
                out-edges))
(defun out-edges (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (nth-value 0 (gethash vertex (graph-vertex-edges graph))))

(declaim (ftype (function (adjacency-graph t)
                          (values list &optional))
                in-edges))
(defun in-edges (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (let ((vertex-hash (graph-vertex-edges graph))
        (vertex-eq (graph-vertex-equality-fn graph)))
    (loop for val being the hash-values of vertex-hash
          nconc (remove-if-not
                 (lambda (edge)
                   (funcall vertex-eq vertex (edge:edge-target edge)))
                 val))))

(declaim (ftype (function (adjacency-graph)
                          (values fixnum &optional))
                vertex-count))
(defun vertex-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (hash-table-count (graph-vertex-edges graph)))

(declaim (ftype (function (adjacency-graph)
                          (values fixnum &optional))
                edge-count))
(defun edge-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (loop for edge-collection of-type list being the hash-values of (graph-vertex-edges graph)
        sum (length edge-collection) into s of-type fixnum
        finally (return s)))

;; looping without malloc

(declaim (ftype (function (adjacency-graph (function (edge:edge)))
                          (values null &optional))
                for-edges))
(defun for-edges (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type function fn))
  (loop for edge-collection being the hash-values of (graph-vertex-edges graph)
        do (dolist (edge edge-collection)
             (funcall fn edge))))

(declaim (ftype (function (adjacency-graph (function (t)))
                          (values null &optional))
                for-vertices))
(defun for-vertices (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type function fn))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        do (funcall fn vertex)))

(declaim (ftype (function (adjacency-graph t (function (edge:edge)))
                          (values null &optional))
                for-out-edges))
(defun for-out-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type adjacency-graph graph))
  (declare (type function fn))
  (dolist (edge (out-edges graph vertex))
    (funcall fn edge)))
