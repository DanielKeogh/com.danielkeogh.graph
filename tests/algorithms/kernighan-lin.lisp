;;;; test/algorithms/kernighan-lin.lisp

(in-package #:com.danielkeogh.graph-tests.algorithms)

(def-suite kernighan-lin-partition-tests
  :description "Tests for kernighan-lin partition algorithm")

(in-suite kernighan-lin-partition-tests)

(defun as-hash (&rest vals)
  (funcall #'alexandria:alist-hash-table (mapcar (lambda (x) (cons x t)) vals)))

(defun partition-eq (partition1-seta partition1-setb partition2-seta partition2-setb)
  (or (and (alexandria:set-equal partition1-seta partition2-seta)
           (alexandria:set-equal partition1-setb partition2-setb))
      (and (alexandria:set-equal partition1-setb partition2-seta)
           (alexandria:set-equal partition1-seta partition2-setb))))

(test empty-graph
  (let* ((graph (api:make-undirected-graph)))
    (multiple-value-bind (seta setb count)
        (alg:kernighan-lin-partition graph 0 (lambda (e) (declare (ignore e)) 0))
      (is (= 0 (hash-table-count seta)))
      (is (= 0 (hash-table-count setb)))
      (is (= 0 count)))))

(test graph1
  (let* ((graph  (api:make-undirected-graph))
         (costs (make-hash-table)))
    (labels ((e (source target weight)
               (let ((edge (api:make-edge source target)))
                 (setf (gethash edge costs) weight)
                 edge)))
      (api:add-edges-and-vertices graph
        (e 0 1 100)
        (e 1 2 20)
        (e 2 3 10)
        (e 1 3 50)))
      
    (multiple-value-bind (seta setb val)
        (alg:kernighan-lin-partition graph 1 (lambda (e) (gethash e costs)))
      (is-true (partition-eq (alexandria:hash-table-keys seta) (alexandria:hash-table-keys setb)
                             '(0 1) '(2 3))))))

(test graph2
  (let* ((graph  (api:make-undirected-graph))
         (costs (make-hash-table)))
    (labels ((e (source target weight)
               (let ((edge (api:make-edge source target)))
                 (setf (gethash edge costs) weight)
                 edge)))
      (api:add-edges-and-vertices graph
        (e 0 1 1)
        (e 1 2 1)
        (e 2 3 1)
        (e 3 4 1)
        (e 0 4 1)
        (e 1 4 1)))
    
    (multiple-value-bind (seta setb val)
        (alg:kernighan-lin-partition graph 1 (lambda (e) (gethash e costs)))
      (is-true (partition-eq (alexandria:hash-table-keys seta) (alexandria:hash-table-keys setb)
                             '(0 1 4) '(2 3))))))

;; TODO: Fix this test?
(test graph3
  (let* ((graph (api:make-undirected-graph))
         (costs (make-hash-table)))
    (labels ((e (source target weight)
               (let ((edge (api:make-edge source target)))
                 (setf (gethash edge costs) weight)
                 edge)))
      (api:add-edges-and-vertices graph
        (e 0 1 1)
        (e 1 2 50)
        (e 1 4 5)
        (e 3 4 1)
        (e 3 6 10)
        (e 4 5 1)
        (e 4 7 25)
        (e 4 8 100)
        (e 5 7 1)
        (e 5 8 3)
        (e 6 7 1)
        (e 6 9 2)
        (e 7 8 1)
        (e 7 10 5)
        (e 8 11 1)))
    
    (multiple-value-bind (seta setb val)
        (alg:kernighan-lin-partition graph 1 (lambda (e) (gethash e costs)))
      (is-true (partition-eq (alexandria:hash-table-keys seta) (alexandria:hash-table-keys setb)
                             '(0 1 2 3 6 9) '(4 5 7 8 10 11))))))
