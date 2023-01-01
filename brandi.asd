(defsystem "brandi"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("serapeum"
               "log4cl"
               "str"
               "yason"
               "cl-dbi"
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"
               "sxql"
               "woo"
               "clack"
               "lack")
  :components ((:module "src"
                :serial nil
                :components
                ((:file "package")
                 (:file "db")
                 (:file "web")
                 (:file "services/github"))))
  :description "BrandI manages my online presence as if I was a brand."
  :in-order-to ((test-op (test-op "brandi/tests"))))

(defsystem "brandi/tests"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("brandi"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "package"))))
  :description "Test system for brandi"
  :perform (test-op (op c) (symbol-call :rove :run c)))
