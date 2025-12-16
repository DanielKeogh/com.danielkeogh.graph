;;;; tests/algorithms/hungarian-assignment.lisp

(in-package #:com.danielkeogh.graph-tests.algorithms)

(def-suite hungarian-assignment-tests
  :description "Tests for hungarian-assignment algorithm")

(in-suite hungarian-assignment-tests)

(defun 2d-arr (arr)
  (let* ((rows (length arr))
         (cols (length (first arr))))
    (make-array (list rows cols) :initial-contents arr)))

(test simple-assignment
  (let* ((matrix (2d-arr '((1 2 3)
                           (3 3 3)
                           (3 3 2))))
         (results (alg:hungarian-assignment matrix)))

    (is (= 0 (aref results 0)))
    (is (= 1 (aref results 1)))
    (is (= 2 (aref results 2)))))

(test job-assignment
  (let* ((matrix (2d-arr '((82 83 69 92)
                           (77 37 49 92)
                           (11 69 5 86)
                           (8 9 98 23))))
         (results (alg:hungarian-assignment matrix)))

    (is (= 2 (aref results 0)))
    (is (= 1 (aref results 1)))
    (is (= 0 (aref results 2)))
    (is (= 3 (aref results 3)))))
