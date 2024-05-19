;;;; api.lisp

(defpackage #:com.danielkeogh.graph
  (:use #:cl)
  (:local-nicknames (#:adjacency #:com.danielkeogh.graph.adjacency)
                    (#:bidirectional #:com.danielkeogh.graph.bidirectional)
                    (#:edge #:com.danielkeogh.graph.edge)
                    (#:utils #:com.danielkeogh.graph.utils))
  (:export
   ;; constructors
   #:make-bidirectional-graph
   #:make-adjacency-graph
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
   #:has-vertex
   #:has-edge
   #:has-edge-between
   #:vertex-equals
   
   ;; graph accessors
   #:for-in-out-edges
   #:for-vertices
   #:for-roots
   #:for-edges

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
   #:graph-equals))

(in-package #:com.danielkeogh.graph)

;;; generics

;; builders

(defgeneric add-vertex (graph v))

(defgeneric add-edge (graph edge)
  (:documentation "Add an edge to a graph. Will error if the edge refers to a vertex that is not yet added to the graph."))

(defgeneric add-edge-between (graph vertex1 vertex2)
  (:documentation "Create a new edge between two vertices on a graph. Will error if the edge refers to a vertex that is not yet added to the graph."))

(defgeneric remove-edge-between (graph vertex1 vertex2)
  (:documentation "Remove all edges edge between two vertexes"))

(defgeneric remove-edge (graph edge)
  (:documentation "Remove all edges edge between two vertexes"))

;; graph accessors

(defgeneric has-vertex (graph vertex)
  (:documentation "See if the vertex has been added."))

(defgeneric out-edges (graph vertex)
  (:documentation "Get outbound edges for a given vertex."))

(defgeneric in-edges (graph vertex)
  (:documentation "Get inbound edges for a given vertex."))

(defgeneric for-vertices (graph fn)
  (:documentation "Apply a function to all verticies in the graph."))
(trivial-indent:define-indentation for-vertices (4 &lambda))

(defgeneric for-edges (graph fn)
  (:documentation "Apply a function to all edges in the graph."))
(trivial-indent:define-indentation for-edges (4 &lambda))

(defgeneric graph-vertex-equality-fn (graph)
  (:documentation "Get the function that checks if two vertices in the graph are the same"))

(defgeneric vertex-count (graph)
  (:documentation "Total count of vertexes in the graph"))

(defgeneric edge-count (graph)
  (:documentation "Total count of edges in the graph"))

;;; impl

;; edge

(defun make-edge (source target)
  (edge:make-edge source target))

;; adjacency

(defun make-adjacency-graph (&key (allow-parallel-edges t) (vertex-equality-fn #'eql))
  (adjacency:make-graph :allow-parallel-edges allow-parallel-edges :vertex-equality-fn vertex-equality-fn))

(defmethod add-edge ((graph adjacency:adjacency-graph) edge)
  (adjacency:add-edge graph edge))

(defmethod add-edge-between ((graph adjacency:adjacency-graph) vertex1 vertex2)
  (adjacency:add-edge-between graph vertex1 vertex2))

(defmethod add-vertex ((graph adjacency:adjacency-graph) vertex)
  (adjacency:add-vertex graph vertex))

(defmethod remove-edge ((graph adjacency:adjacency-graph) edge)
  (adjacency:remove-edge graph edge))

(defmethod remove-edge-between ((graph adjacency:adjacency-graph) vertex1 vertex2)
  (adjacency:remove-edge-between graph vertex1 vertex2))

(defmethod remove-vertex ((graph adjacency:adjacency-graph) vertex)
  (adjacency:remove-vertex graph vertex))

(defmethod in-edges ((graph adjacency:adjacency-graph) vertex)
  (adjacency:in-edges graph vertex))

(defmethod out-edges ((graph adjacency:adjacency-graph) vertex)
  (adjacency:out-edges graph vertex))

(defmethod graph-vertex-equality-fn ((graph adjacency:adjacency-graph))
  (adjacency:graph-vertex-equality-fn graph))

(defmethod for-vertices ((graph adjacency:adjacency-graph) fn)
  (adjacency:for-vertices graph fn))

(defmethod for-edges ((graph adjacency:adjacency-graph) fn)
  (adjacency:for-edges graph fn))

(defmethod has-vertex ((graph adjacency:adjacency-graph) vertex)
  (adjacency:has-vertex graph vertex))

(defmethod has-edge ((graph adjacency:adjacency-graph) edge)
  (adjacency:has-edge graph edge))

(defmethod has-edge-between ((graph adjacency:adjacency-graph) source target)
  (adjacency:has-edge-between graph source target))

(defmethod vertex-count ((graph adjacency:adjacency-graph))
  (adjacency:vertex-count graph))

(defmethod edge-count ((graph adjacency:adjacency-graph))
  (adjacency:edge-count graph))

;; bidirectional

(defun make-bidirectional-graph (&key (allow-parallel-edges t) (vertex-equality-fn #'eql))
  (bidirectional:make-graph :allow-parallel-edges allow-parallel-edges :vertex-equality-fn vertex-equality-fn))

(defmethod add-edge ((graph bidirectional:bidirectional-graph) edge)
  (bidirectional:add-edge graph edge))

(defmethod add-edge-between ((graph bidirectional:bidirectional-graph) vertex1 vertex2)
  (bidirectional:add-edge-between graph vertex1 vertex2))

(defmethod add-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:add-vertex graph vertex))

(defmethod remove-edge ((graph bidirectional:bidirectional-graph) edge)
  (bidirectional:remove-edge graph edge))

(defmethod remove-edge-between ((graph bidirectional:bidirectional-graph) vertex1 vertex2)
  (bidirectional:remove-edge-between graph vertex1 vertex2))

(defmethod remove-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:remove-vertex graph vertex))

(defmethod in-edges ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:in-edges graph vertex))

(defmethod out-edges ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:out-edges graph vertex))

(defmethod graph-vertex-equality-fn ((graph bidirectional:bidirectional-graph))
  (bidirectional:graph-vertex-equality-fn graph))

(defmethod for-vertices ((graph bidirectional:bidirectional-graph) fn)
  (bidirectional:for-vertices graph fn))

(defmethod for-edges ((graph bidirectional:bidirectional-graph) fn)
  (bidirectional:for-edges graph fn))

(defmethod has-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:has-vertex graph vertex))

(defmethod has-edge ((graph bidirectional:bidirectional-graph) edge)
  (bidirectional:has-edge graph edge))

(defmethod has-edge-between ((graph bidirectional:bidirectional-graph) source target)
  (bidirectional:has-edge-between graph source target))

(defmethod vertex-count ((graph bidirectional:bidirectional-graph))
  (bidirectional:vertex-count graph))

(defmethod edge-count ((graph bidirectional:bidirectional-graph))
  (bidirectional:edge-count graph))

;; edge accessors

(defun edge-source (edge)
  (edge:edge-source edge))

(defun edge-target (edge)
  (edge:edge-target edge))

;; utils

(defun add-vertices (graph &rest vertices)
  (dolist (vertex vertices)
    (add-vertex graph vertex)))
(trivial-indent:define-indentation add-vertices (4 &body))

(defun add-edges (graph &rest edges)
  (dolist (edge edges)
    (add-edge graph edge)))
(trivial-indent:define-indentation add-edges (4 &body))

(defun add-edges-and-vertices (graph &rest edges)
  "Add a collection of edges to the graph, ensuring that their vertices are also added"
  (dolist (edge edges)
    (let ((source (edge-source edge))
          (target (edge-target edge)))
      (add-vertex graph source)
      (add-vertex graph target)
      (add-edge graph edge))))

(trivial-indent:define-indentation add-edges-and-vertices (4 &body))

(defun add-edges-and-vertices-between (graph &rest pairs)
  (loop for (source target) on pairs by #'cddr do
    (add-vertex graph source)
    (add-vertex graph target)
    (add-edge-between graph source target)))

(defun vertices (graph)
  "Get all verticies in the graph as a list."
  (utils:with-collector (collect)
    (for-vertices graph #'collect)))

(defun edges (graph)
  "Get all edges in the graph as a list."
  (utils:with-collector (collect)
    (for-edges graph #'collect)))

(defun for-roots (graph fn)
  "Call a function on all vertices with no inbound edges."
  (for-vertices graph (lambda (vertex)
                        (unless (in-edges graph vertex)
                          (funcall fn vertex)))))
(trivial-indent:define-indentation for-roots (4 &lambda))

(defun roots (graph)
  "Get all vertices with no inbound edges."
  (utils:with-collector (collect)
    (for-roots graph #'collect)))

(defun for-in-out-edges (graph vertex fn)
  (loop for edge in (in-edges graph vertex)
    do (funcall fn edge))
  (loop for edge in (out-edges graph vertex)
        do (funcall fn edge)))
(trivial-indent:define-indentation for-in-out-edges (4 4 &lambda))

(defun vertex-equals (graph vertex1 vertex2)
  (funcall (graph-vertex-equality-fn graph) vertex1 vertex2))

;;; with-graph* utils

(defvar *graph*)

(defmacro with-graph* ((graph) &body body)
  `(let ((*graph* ,graph))
     ,@body
     *graph*))

(defun add-edge* (edge)
  (add-edge *graph* edge))

(defun add-vertex* (vertex)
  (add-vertex *graph* vertex))

(defun add-edges-and-vertices* (&rest edges)
  (apply #'add-edges-and-vertices (cons *graph* edges)))

(defun add-edges-and-vertices-between* (&rest pairs)
  (apply #'add-edges-and-vertices-between (cons *graph* pairs)))

;;; utils

(defun pretty-print (graph &optional (stream t))
  (format stream "VERTICES: ~{~a~^, ~}~%EDGES: ~{~a~^, ~}~%" (vertices graph) (edges graph)))

(defun graph-equals (graph1 graph2)
  (and (= (vertex-count graph1)
          (vertex-count graph2))
       (eq (graph-vertex-equality-fn graph1)
           (graph-vertex-equality-fn graph2))
       (= (edge-count graph1)
          (edge-count graph2))
       (loop for vertex in (vertices graph1)
             always (and (has-vertex graph2 vertex)
                         (loop for edge in (out-edges graph1 vertex)
                               always (has-edge-between graph2 (edge-source edge) (edge-target edge)))))))
