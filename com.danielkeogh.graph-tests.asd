;;;; com.danielkeogh.graph-tests.asd

(asdf:defsystem #:com.danielkeogh.graph-tests
  :description "Tests for com.danielkeogh.graph"
  :author "Daniel Keogh"
  :licence "MIT"
  :depends-on (:com.danielkeogh.graph :fiveam)
  :components ((:module "tests"
                :pathname "tests"
                :components
                ((:module "structures"
                  :pathname "structures"
                  :components ((:file "package")
                               (:file "bidirectional")
                               (:file "api")))
                 (:module "algorithms"
                  :pathname "algorithms"
                  :components ((:file "package")
                               (:file "depth-first-search")
                               (:file "breadth-first-search")
                               (:file "bidirectional-breadth-first-search")
                               (:file "minimum-spanning-tree")))))))
