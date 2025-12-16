;;;; com.danielkeogh.graph-tests.asd

(asdf:defsystem #:com.danielkeogh.graph-tests
  :description "Tests for com.danielkeogh.graph"
  :author "Daniel Keogh"
  :licence "MIT"
  :depends-on (:com.danielkeogh.graph :fiveam)
  :components ((:module "tests"
                :pathname "tests"
                :components
                ((:file "package")
                 (:module "structures"
                  :pathname "structures"
                  :components ((:file "package")
                               (:file "bidirectional")
                               (:file "api")))
                 (:module "algorithms"
                  :pathname "algorithms"
                  :components ((:file "package")
                               ;; assignment
                               (:file "hungarian-assignment")
                               ;; search
                               (:file "bidirectional-breadth-first-search")
                               (:file "breadth-first-search")
                               (:file "depth-first-search")
                               ;; connected components
                               (:file "strongly-connected-components")
                               (:file "weakly-connected-components")
                               ;; condensation
                               (:file "condensate-vertices")
                               ;; graph partition
                               (:file "kernighan-lin")
                               ;; ??
                               (:file "minimum-spanning-tree")))))))
