;;;; api.lisp

(defpackage #:com.danielkeogh.graph
  (:use #:cl)
  (:local-nicknames (#:adjacency #:com.danielkeogh.graph.adjacency)
                    (#:bidirectional #:com.danielkeogh.graph.bidirectional)
                    (#:bidirectional-matrix #:com.danielkeogh.graph.bidirectional-matrix)
                    (#:undirected #:com.danielkeogh.graph.undirected)
                    (#:edge #:com.danielkeogh.graph.edge)
                    (#:utils #:com.danielkeogh.graph.utils))
  (:export
   ;; constructors
   #:make-adjacency-graph
   #:make-bidirectional-graph
   #:make-bidirectional-matrix-graph
   #:make-undirected-graph
   #:make-edge

   ;; builders
   #:add-vertex
   #:add-vertices
   #:add-edge
   #:add-edges
   #:add-edges-and-vertices
   #:add-edges-and-vertices-between
   #:add-edge-between
   #:remove-vertex
   #:remove-edge
   #:remove-edge-between

   ;; tests
   #:is-directed
   #:has-vertex
   #:has-edge
   #:has-edge-between
   #:vertex-equals
   
   ;; graph accessors
   #:for-in-out-edges
   #:for-vertices
   #:for-roots
   #:for-edges

   #:adjacent-edges
   #:out-edges
   #:in-edges
   #:vertices
   #:roots
   #:edges

   #:vertex-count
   #:edge-count

   #:graph-vertex-equality-fn
   
   ;; edge accessors
   #:edge-source
   #:edge-target
   
   ;; dynamic builders
   #:with-graph*
   #:add-edge*
   #:add-vertex*
   #:add-edges-and-vertices*
   #:add-edges-and-vertices-between*

   ;; utils
   #:pretty-print
   #:graph-equals

   ;; conditions
   #:unsupported-generic))

(in-package #:com.danielkeogh.graph)

;;; conditions

(define-condition unsupported-generic (error)
  ((supported-by :initarg :supported-by
                 :accessor unsupported-generic-supported-by))
  (:report (lambda (condition stream)
             (format stream
                     "Method is invalid for this type of graph. It is only supported by ~S.~&"
                     (unsupported-generic-supported-by condition))))
  (:documentation "This graph cannot support this generic."))

;;; generics

;; builders

(defgeneric add-vertex (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Add a vertex to a graph."))

(defgeneric add-edge (graph edge)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Add an edge to a graph. Will error if the edge refers to a vertex that is not yet added to the graph."))

(defgeneric add-edge-between (graph vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Create a new edge between two vertices on a graph. Will error if the edge refers to a vertex that is not yet added to the graph."))

(defgeneric remove-edge-between (graph vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Remove all edges edge between two vertexes"))

(defgeneric remove-edge (graph edge)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Remove all edges edge between two vertexes"))

;; graph accessors

(defgeneric has-vertex (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "See if the vertex has been added."))

(defgeneric adjacent-edges (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Get adjacent edges for a given vertex. Only supported for undirected graphs."))

(defgeneric out-edges (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Get outbound edges for a given vertex. Only supported for directed graphs."))

(defgeneric in-edges (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Get inbound edges for a given vertex. Only supported for directed graphs."))

(defgeneric for-vertices (graph fn)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Apply a function to all verticies in the graph."))
(trivial-indent:define-indentation for-vertices (4 &lambda))

(defgeneric for-edges (graph fn)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Apply a function to all edges in the graph."))
(trivial-indent:define-indentation for-edges (4 &lambda))

(defgeneric graph-vertex-equality-fn (graph)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Get the function that checks if two vertices in the graph are the same"))

(defgeneric vertex-count (graph)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Total count of vertexes in the graph"))

(defgeneric edge-count (graph)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Total count of edges in the graph"))

(defgeneric is-directed (graph)
  (declare #.utils:*external-optimize-settings*)
  (:documentation "Is it a directed graph?"))

;;; unsupported

(defmethod in-edges (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (error 'unsupported-generic :supported-by "directed graphs"))

(defmethod out-edges (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (error 'unsupported-generic :supported-by "directed graphs"))

(defmethod adjacent-edges (graph vertex)
  (declare #.utils:*external-optimize-settings*)
  (error 'unsupported-generic :supported-by "undirected graphs"))

;;; impl

;; edge

(declaim (ftype (function (t t) (values edge:edge &optional)) make-edge))
(defun make-edge (source target)
  "Create an edge between two vertices. This edge may be used in both directed and undirected graphs."
  (declare #.utils:*external-optimize-settings*)
  (edge:make-edge source target))

;; adjacency

(declaim (ftype (function * (values adjacency:adjacency-graph &optional))
                make-adjacency-graph))
(defun make-adjacency-graph (&key (allow-parallel-edges t) (vertex-equality-fn #'eql))
  "Create a directed graph optimized for `out-edges` access."
  (declare #.utils:*external-optimize-settings*)
  (adjacency:make-graph :allow-parallel-edges allow-parallel-edges :vertex-equality-fn vertex-equality-fn))

(defmethod add-edge ((graph adjacency:adjacency-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:add-edge graph edge))

(defmethod add-edge-between ((graph adjacency:adjacency-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:add-edge-between graph vertex1 vertex2))

(defmethod add-vertex ((graph adjacency:adjacency-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:add-vertex graph vertex))

(defmethod remove-edge ((graph adjacency:adjacency-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:remove-edge graph edge))

(defmethod remove-edge-between ((graph adjacency:adjacency-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:remove-edge-between graph vertex1 vertex2))

(defmethod remove-vertex ((graph adjacency:adjacency-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:remove-vertex graph vertex))

(defmethod in-edges ((graph adjacency:adjacency-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:in-edges graph vertex))

(defmethod out-edges ((graph adjacency:adjacency-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:out-edges graph vertex))

(defmethod graph-vertex-equality-fn ((graph adjacency:adjacency-graph))
  (declare #.utils:*external-optimize-settings*)
  (adjacency:graph-vertex-equality-fn graph))

(defmethod for-vertices ((graph adjacency:adjacency-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:for-vertices graph fn))

(defmethod for-edges ((graph adjacency:adjacency-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:for-edges graph fn))

(defmethod is-directed ((graph adjacency:adjacency-graph))
  (declare #.utils:*external-optimize-settings*)
  t)

(defmethod has-vertex ((graph adjacency:adjacency-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:has-vertex graph vertex))

(defmethod has-edge ((graph adjacency:adjacency-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:has-edge graph edge))

(defmethod has-edge-between ((graph adjacency:adjacency-graph) source target)
  (declare #.utils:*external-optimize-settings*)
  (adjacency:has-edge-between graph source target))

(defmethod vertex-count ((graph adjacency:adjacency-graph))
  (declare #.utils:*external-optimize-settings*)
  (adjacency:vertex-count graph))

(defmethod edge-count ((graph adjacency:adjacency-graph))
  (declare #.utils:*external-optimize-settings*)
  (adjacency:edge-count graph))

;; bidirectional

(declaim (ftype (function * (values bidirectional:bidirectional-graph &optional))
                make-bidirectional-graph))
(defun make-bidirectional-graph (&key (allow-parallel-edges t) (vertex-equality-fn #'eql))
  "Create a directed graph optimized for `out-edges` and `in-edges` access."
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:make-graph :allow-parallel-edges allow-parallel-edges :vertex-equality-fn vertex-equality-fn))

(defmethod add-edge ((graph bidirectional:bidirectional-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:add-edge graph edge))

(defmethod add-edge-between ((graph bidirectional:bidirectional-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:add-edge-between graph vertex1 vertex2))

(defmethod add-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:add-vertex graph vertex))

(defmethod remove-edge ((graph bidirectional:bidirectional-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:remove-edge graph edge))

(defmethod remove-edge-between ((graph bidirectional:bidirectional-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:remove-edge-between graph vertex1 vertex2))

(defmethod remove-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:remove-vertex graph vertex))

(defmethod in-edges ((graph bidirectional:bidirectional-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:in-edges graph vertex))

(defmethod out-edges ((graph bidirectional:bidirectional-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:out-edges graph vertex))

(defmethod graph-vertex-equality-fn ((graph bidirectional:bidirectional-graph))
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:graph-vertex-equality-fn graph))

(defmethod for-vertices ((graph bidirectional:bidirectional-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:for-vertices graph fn))

(defmethod for-edges ((graph bidirectional:bidirectional-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:for-edges graph fn))

(defmethod is-directed ((graph bidirectional:bidirectional-graph))
  (declare #.utils:*external-optimize-settings*)
  t)

(defmethod has-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:has-vertex graph vertex))

(defmethod has-edge ((graph bidirectional:bidirectional-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:has-edge graph edge))

(defmethod has-edge-between ((graph bidirectional:bidirectional-graph) source target)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:has-edge-between graph source target))

(defmethod vertex-count ((graph bidirectional:bidirectional-graph))
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:vertex-count graph))

(defmethod edge-count ((graph bidirectional:bidirectional-graph))
  (declare #.utils:*external-optimize-settings*)
  (bidirectional:edge-count graph))

;; bidirectional-matrix

(declaim (ftype (function * (values bidirectional-matrix:bidirectional-matrix-graph
                                    &optional))
                make-bidirectional-matrix-graph))
(defun make-bidirectional-matrix-graph (vertex-count)
  "Create a graph with pre-populated vertices optimized for checking if a given edge exists. Vertices are represented as `fixnum` from 0 below `vertex-count`."
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:make-graph vertex-count))

(defmethod add-edge ((graph bidirectional-matrix:bidirectional-matrix-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:add-edge graph edge))

(defmethod add-edge-between ((graph bidirectional-matrix:bidirectional-matrix-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:add-edge-between graph vertex1 vertex2))

(defmethod add-vertex ((graph bidirectional-matrix:bidirectional-matrix-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (unless (bidirectional-matrix:has-vertex graph vertex)
    (error "bidirectional-matrix-graph cannot support adding of new vertices")))

(defmethod remove-edge ((graph bidirectional-matrix:bidirectional-matrix-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:remove-edge graph edge))

(defmethod remove-edge-between ((graph bidirectional-matrix:bidirectional-matrix-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:remove-edge-between graph vertex1 vertex2))

(defmethod remove-vertex ((graph bidirectional-matrix:bidirectional-matrix-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (error "bidirectional-matrix-graph cannot support removal of vertices"))

(defmethod in-edges ((graph bidirectional-matrix:bidirectional-matrix-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:in-edges graph vertex))

(defmethod out-edges ((graph bidirectional-matrix:bidirectional-matrix-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:out-edges graph vertex))

(defmethod graph-vertex-equality-fn ((graph bidirectional-matrix:bidirectional-matrix-graph))
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:graph-vertex-equality-fn graph))

(defmethod for-vertices ((graph bidirectional-matrix:bidirectional-matrix-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:for-vertices graph fn))

(defmethod for-edges ((graph bidirectional-matrix:bidirectional-matrix-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:for-edges graph fn))

(defmethod is-directed ((graph bidirectional-matrix:bidirectional-matrix-graph))
  (declare #.utils:*external-optimize-settings*)
  t)

(defmethod has-vertex ((graph bidirectional-matrix:bidirectional-matrix-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:has-vertex graph vertex))

(defmethod has-edge ((graph bidirectional-matrix:bidirectional-matrix-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:has-edge graph edge))

(defmethod has-edge-between ((graph bidirectional-matrix:bidirectional-matrix-graph) source target)
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:has-edge-between graph source target))

(defmethod vertex-count ((graph bidirectional-matrix:bidirectional-matrix-graph))
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:vertex-count graph))

(defmethod edge-count ((graph bidirectional-matrix:bidirectional-matrix-graph))
  (declare #.utils:*external-optimize-settings*)
  (bidirectional-matrix:edge-count graph))

;; undirected

(declaim (ftype (function * (values undirected:undirected-graph &optional))
                make-undirected-graph))
(defun make-undirected-graph (&key
                                (allow-parallel-edges t)
                                (vertex-equality-fn #'eql))
  "Create an undirected graph optimized for finding edges of a given vertex quickly."
  (declare #.utils:*external-optimize-settings*)
  (undirected:make-graph :allow-parallel-edges allow-parallel-edges
                         :vertex-equality-fn vertex-equality-fn))

(defmethod add-edge ((graph undirected:undirected-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (undirected:add-edge graph edge))

(defmethod add-edge-between ((graph undirected:undirected-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (undirected:add-edge-between graph vertex1 vertex2))

(defmethod add-vertex ((graph undirected:undirected-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (undirected:add-vertex graph vertex))

(defmethod remove-edge ((graph undirected:undirected-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (undirected:remove-edge graph edge))

(defmethod remove-edge-between ((graph undirected:undirected-graph) vertex1 vertex2)
  (declare #.utils:*external-optimize-settings*)
  (undirected:remove-edge-between graph vertex1 vertex2))

(defmethod remove-vertex ((graph undirected:undirected-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (undirected:remove-vertex graph vertex))

(defmethod adjacent-edges ((graph undirected:undirected-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (undirected:adjacent-edges graph vertex))

(defmethod graph-vertex-equality-fn ((graph undirected:undirected-graph))
  (declare #.utils:*external-optimize-settings*)
  (undirected:graph-vertex-equality-fn graph))

(defmethod for-vertices ((graph undirected:undirected-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (undirected:for-vertices graph fn))

(defmethod for-edges ((graph undirected:undirected-graph) fn)
  (declare #.utils:*external-optimize-settings*)
  (undirected:for-edges graph fn))

(defmethod is-directed ((graph undirected:undirected-graph))
  (declare #.utils:*external-optimize-settings*)
  nil)

(defmethod has-vertex ((graph undirected:undirected-graph) vertex)
  (declare #.utils:*external-optimize-settings*)
  (undirected:has-vertex graph vertex))

(defmethod has-edge ((graph undirected:undirected-graph) edge)
  (declare #.utils:*external-optimize-settings*)
  (undirected:has-edge graph edge))

(defmethod has-edge-between ((graph undirected:undirected-graph) source target)
  (declare #.utils:*external-optimize-settings*)
  (undirected:has-edge-between graph source target))

(defmethod vertex-count ((graph undirected:undirected-graph))
  (declare #.utils:*external-optimize-settings*)
  (undirected:vertex-count graph))

(defmethod edge-count ((graph undirected:undirected-graph))
  (declare #.utils:*external-optimize-settings*)
  (undirected:edge-count graph))

;; edge accessors

(defun edge-source (edge)
  "Find the source vertex of an edge. For undirected graphs the orientation is arbitrary."
  (declare #.utils:*external-optimize-settings*)
  (edge:edge-source edge))

(defun edge-target (edge)
  "Find the target vertex of an edge. For undirected graphs the orientation is arbitrary."
  (declare #.utils:*external-optimize-settings*)
  (edge:edge-target edge))

;; utils

(defun add-vertices (graph &rest vertices)
  "Add a set of vertices to a graph."
  (declare #.utils:*external-optimize-settings*)
  (dolist (vertex vertices)
    (add-vertex graph vertex)))
(trivial-indent:define-indentation add-vertices (4 &body))

(defun add-edges (graph &rest edges)
  "Add a set of edges to a graph."
  (declare #.utils:*external-optimize-settings*)
  (dolist (edge edges)
    (add-edge graph edge)))
(trivial-indent:define-indentation add-edges (4 &body))

(defun add-edges-and-vertices (graph &rest edges)
  "Add a collection of edges to the graph, ensuring that their vertices are also added."
  (declare #.utils:*external-optimize-settings*)
  (dolist (edge edges)
    (let ((source (edge-source edge))
          (target (edge-target edge)))
      (add-vertex graph source)
      (add-vertex graph target)
      (add-edge graph edge))))

(trivial-indent:define-indentation add-edges-and-vertices (4 &body))

(defun add-edges-and-vertices-between (graph &rest pairs)
  "Convert a p-list into a set of edges and add all of these edges to a graph, ensuring their vertices are also added."
  (declare #.utils:*external-optimize-settings*)
  (loop for (source target) on pairs by #'cddr do
    (add-vertex graph source)
    (add-vertex graph target)
    (add-edge-between graph source target)))

(defun vertices (graph)
  "Get all verticies in the graph as a list."
  (declare #.utils:*external-optimize-settings*)
  (utils:with-collector (collect)
    (for-vertices graph #'collect)))

(defun edges (graph)
  "Get all edges in the graph as a list."
  (declare #.utils:*external-optimize-settings*)
  (utils:with-collector (collect)
    (for-edges graph #'collect)))

(defun for-roots (graph fn)
  "Call a function on all vertices with no inbound edges."
  (declare #.utils:*external-optimize-settings*)
  (declare (type (function (t) t) fn))
  (for-vertices graph (lambda (vertex)
                        (unless (in-edges graph vertex)
                          (funcall fn vertex)))))
(trivial-indent:define-indentation for-roots (4 &lambda))

(defun roots (graph)
  "Get all vertices with no inbound edges."
  (declare #.utils:*external-optimize-settings*)
  (utils:with-collector (collect)
    (for-roots graph #'collect)))

(defun for-in-out-edges (graph vertex fn)
  "Apply a function `fn` to each edge to and from a vertex in a directed graph."
  (declare #.utils:*external-optimize-settings*)
  (declare (type (function (edge:edge) t) fn))
  (loop for edge in (in-edges graph vertex)
    do (funcall fn edge))
  (loop for edge in (out-edges graph vertex)
        do (funcall fn edge)))
(trivial-indent:define-indentation for-in-out-edges (4 4 &lambda))

(defun vertex-equals (graph vertex1 vertex2)
  "Check if two vertices are considered equal in a graph."
  (declare #.utils:*external-optimize-settings*)
  (funcall (the (function (t t) boolean)
                (graph-vertex-equality-fn graph))
           vertex1 vertex2))

;;; with-graph* utils

(defvar *graph*)

(defmacro with-graph* ((graph) &body body)
  "Set the dynamic variable *graph* for use in dynamic builder functions."
  `(let ((*graph* ,graph))
     ,@body
     *graph*))

(defun add-edge* (edge)
  "Add an edge to *graph*."
  (declare #.utils:*external-optimize-settings*)
  (add-edge *graph* edge))

(defun add-vertex* (vertex)
  "Add a vertex to *graph*."
  (declare #.utils:*external-optimize-settings*)
  (add-vertex *graph* vertex))

(defun add-edges-and-vertices* (&rest edges)
  "Add a set of edges to *graph*, ensuring that their vertices are also added."
  (declare #.utils:*external-optimize-settings*)
  (apply #'add-edges-and-vertices (cons *graph* edges)))

(defun add-edges-and-vertices-between* (&rest pairs)
  "Convert a p-list into a set of edges and add all of these edges to *graph*, ensuring their vertices are also added."
  (declare #.utils:*external-optimize-settings*)
  (apply #'add-edges-and-vertices-between (cons *graph* pairs)))

;;; utils

(defun pretty-print (graph &optional (stream t))
  "Print the set of vertices and edges in a graph."
  (format stream "VERTICES: ~{~a~^, ~}~%EDGES: ~{~a~^, ~}~%" (vertices graph) (edges graph)))

(defun graph-equals (graph1 graph2)
  "Check if two graphs contain the same sets of edges and vertices."
  (declare #.utils:*external-optimize-settings*)
  (and (= (the fixnum (vertex-count graph1))
          (the fixnum (vertex-count graph2)))
       (eq (graph-vertex-equality-fn graph1)
           (graph-vertex-equality-fn graph2))
       (= (the fixnum (edge-count graph1))
          (the fixnum (edge-count graph2)))
       (loop for vertex in (vertices graph1)
             always (and (has-vertex graph2 vertex)
                         (loop for edge in (out-edges graph1 vertex)
                               always (has-edge-between graph2 (edge-source edge) (edge-target edge)))))))
