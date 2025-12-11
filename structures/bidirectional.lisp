;;;; bidirectional.lisp

(defpackage #:com.danielkeogh.graph.bidirectional
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
   #:out-edges
   #:in-edges
   #:vertex-count
   #:edge-count
   #:clone

   ;; accessors
   #:graph-vertex-equality-fn

   ;; looping without malloc
   #:for-edges
   #:for-in-out-eges
   #:for-vertices
   #:for-out-edges
   #:for-in-edges

   ;; types
   #:bidirectional-graph
   #:bidirectional-graph-p))

(in-package #:com.danielkeogh.graph.bidirectional)

;;; struct definitions

(defstruct (bidirectional-graph (:conc-name graph-))
  (allow-parallel-edges (utils:required-argument "allow-parallel-edges") :type boolean :read-only t)
  (vertex-in-edges (utils:required-argument "vertex-in-edges") :type hash-table :read-only t)
  (vertex-out-edges (utils:required-argument "vertex-out-edges") :type hash-table :read-only t)
  (vertex-equality-fn (utils:required-argument "vertex-equality-fn") :type function :read-only t))

;;; constructors

(declaim (ftype (function (&key (:allow-parallel-edges boolean)
                                (:vertex-equality-fn (function (t t) boolean)))
                          (values bidirectional-graph &optional))
                make-graph))
(defun make-graph (&key
                     (allow-parallel-edges t)
                     (vertex-equality-fn #'eql))
  (declare #.utils:*internal-optimize-settings*)
  (make-bidirectional-graph
   :allow-parallel-edges allow-parallel-edges
   :vertex-in-edges (make-hash-table :test vertex-equality-fn)
   :vertex-out-edges (make-hash-table :test vertex-equality-fn)
   :vertex-equality-fn vertex-equality-fn))

(defun make-edge-list ()
  (declare #.utils:*internal-optimize-settings*)
  (list))

;; macros

(defmacro ensure-vertex (graph vertex)
  `(unless (has-vertex ,graph ,vertex)
     (error "Unrecognized ~S vertex ~S. Call add-vertex first." (prin1-to-string ',vertex) ,vertex)))

;; utils

(declaim (ftype (function (bidirectional-graph edge:edge t t)
                          (values boolean &optional))
                edge-equal))
(defun edge-equal (graph edge source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type edge:edge edge))
  (let ((test (graph-vertex-equality-fn graph)))
    (and (funcall test source (edge:edge-source edge))
         (funcall test target (edge:edge-target edge)))))

;; api

(declaim (ftype (function (bidirectional-graph t)
                          (values boolean &optional))
                has-vertex))
(defun has-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (when (nth-value 1 (gethash vertex (graph-vertex-out-edges graph)))
    t)) ;; return boolean type for speed and to protect against memory leaks

(declaim (ftype (function (bidirectional-graph edge:edge)
                          (values boolean &optional))
                has-edge))
(defun has-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type edge:edge edge))
  (let ((edges (gethash (edge:edge-source edge) (graph-vertex-out-edges graph))))
    (declare (type (or null list) edges))
    (when edges
      (find edge edges :test #'eql))))

(declaim (ftype (function (bidirectional-graph t t)
                          (values boolean &optional))
                has-edge-between))
(defun has-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (let ((edges (gethash source (graph-vertex-out-edges graph))))
    (declare (type (or null list) edges))
    (when edges
      (loop for edge in edges
              thereis (edge-equal graph edge source target)))))

(declaim (ftype (function (bidirectional-graph t)
                          (values boolean &optional))
                add-vertex))
(defun add-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (unless (has-vertex graph vertex)
    (setf (gethash vertex (graph-vertex-in-edges graph)) (make-edge-list)
          (gethash vertex (graph-vertex-out-edges graph)) (make-edge-list))
    t))

(declaim (ftype (function (bidirectional-graph edge:edge)
                          (values (or edge:edge null) &optional))
                %add-edge))
(defun %add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type edge:edge edge))
  (labels ((add-edge-to-hash (hashtable vertex edge)
             (push edge (gethash vertex hashtable))))
    (let ((source (edge:edge-source edge))
          (target (edge:edge-target edge)))
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (when (or (graph-allow-parallel-edges graph)
                (not (has-edge-between graph source target)))
        (add-edge-to-hash (graph-vertex-out-edges graph) source edge)
        (add-edge-to-hash (graph-vertex-in-edges graph) target edge)
        edge))))

(declaim (ftype (function (bidirectional-graph edge:edge)
                          (values (or edge:edge null) &optional))
                add-edge))
(defun add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type edge:edge edge))
  (%add-edge graph edge))

(declaim (ftype (function (bidirectional-graph t t)
                          (values (or edge:edge null) &optional))
                add-edge-between))
(defun add-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (%add-edge graph (edge:make-edge source target)))

(declaim (ftype (function (bidirectional-graph edge:edge)
                          (values null &optional))
                remove-between))
(defun remove-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type edge:edge edge))
  (let ((out (graph-vertex-out-edges graph))
        (in (graph-vertex-in-edges graph)))
    (edge:with-edge (source target) edge
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (setf (gethash source out) (remove edge (gethash source out))
            (gethash target in) (remove edge (gethash target in))))
    nil))

(declaim (ftype (function (bidirectional-graph t t)
                          (values null &optional))
                remove-edge-between))
(defun remove-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (ensure-vertex graph source)
  (ensure-vertex graph target)
  (let ((out (graph-vertex-out-edges graph))
        (in (graph-vertex-in-edges graph)))
    (let ((should-remove-edge (lambda (edge) (edge-equal graph edge source target))))
      (setf (gethash source out) (remove-if should-remove-edge (gethash source out))
            (gethash target in) (remove-if should-remove-edge (gethash target in)))))
  nil)

(declaim (ftype (function (bidirectional-graph t)
                          (values null &optional))
                remove-vertex))
(defun remove-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (let ((in (graph-vertex-in-edges graph))
        (out (graph-vertex-out-edges graph)))
    (loop for edge in (gethash vertex in)
          do (setf (gethash (edge:edge-source edge) out)
                   (remove edge (gethash (edge:edge-source edge) out))))
    
    (loop for edge in (gethash vertex out)
          do (setf (gethash (edge:edge-target edge) in)
                   (remove edge (gethash (edge:edge-target edge) in))))

    (remhash vertex in)
    (remhash vertex out))
  nil)

;;; utils

(declaim (ftype (function (bidirectional-graph)
                          (values list &optional))
                edges))
(defun edges (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (loop for edge-collection being the hash-values of (graph-vertex-out-edges graph)
        nconc (copy-list edge-collection)))

(declaim (ftype (function (bidirectional-graph)
                          (values list &optional))
                vertices))
(defun vertices (graph)
  (declare (type bidirectional-graph graph)
           (optimize (speed 3) (safety 0)))
  (loop for vertex being the hash-keys of (graph-vertex-out-edges graph)
        collect vertex))

(declaim (ftype (function (bidirectional-graph t)
                          (values list &optional))
                out-edges))
(defun out-edges (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (nth-value 0 (gethash vertex (graph-vertex-out-edges graph))))

(declaim (ftype (function (bidirectional-graph t)
                          (values list &optional))
                in-edges))
(defun in-edges (graph vertex)
  (declare (type bidirectional-graph graph))
  (nth-value 0 (gethash vertex (graph-vertex-in-edges graph))))

(declaim (ftype (function (bidirectional-graph)
                          (values fixnum &optional))
                vertex-count))
(defun vertex-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (hash-table-count (graph-vertex-out-edges graph)))

(declaim (ftype (function (bidirectional-graph)
                          (values fixnum &optional))
                edge-count))
(defun edge-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (loop for edge-collection of-type list being the hash-values of (graph-vertex-out-edges graph)
        sum (length edge-collection) into j of-type fixnum
        finally (return j)))

(declaim (ftype (function (bidirectional-graph)
                          (values bidirectional-graph &optional))
                clone))
(defun clone (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (let ((clone (make-graph
                :allow-parallel-edges (graph-allow-parallel-edges graph)
                :vertex-equality-fn (graph-vertex-equality-fn graph))))
    (dolist (vertex (vertices graph))
      (add-vertex clone vertex))
    (dolist (edge (edges graph))
      (add-edge-between clone (edge:edge-source edge) (edge:edge-target edge)))
    clone))

;;; looping without malloc

(declaim (ftype (function (bidirectional-graph (function (edge:edge)))
                          (values null &optional))
                for-edges))
(defun for-edges (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type function fn))
  (loop for edge-collection being the hash-values of (graph-vertex-out-edges graph)
        do (loop for edge in edge-collection
                 do (funcall fn edge))))

(declaim (ftype (function (bidirectional-graph (function (t)))
                          (values null &optional))
                for-vertices))
(defun for-vertices (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type function fn))
  (loop for vertex being the hash-keys of (graph-vertex-out-edges graph)
        do (funcall fn vertex)))

(declaim (ftype (function (bidirectional-graph t (function (edge:edge)))
                          (values null &optional))
                for-out-edges))
(defun for-out-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type function fn))
  (loop for edge in (out-edges graph vertex)
        do (funcall fn edge)))

(declaim (ftype (function (bidirectional-graph t (function (edge:edge)))
                          (values null &optional))
                for-in-edges))
(defun for-in-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type function fn))
  (loop for edge in (in-edges graph vertex)
        do (funcall fn edge)))

(declaim (ftype (function (bidirectional-graph t (function (edge:edge)))
                          (values null &optional))
                for-in-out-edges))
(defun for-in-out-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-graph graph))
  (declare (type function fn))
  (for-in-edges graph vertex fn)
  (for-out-edges graph vertex fn))
