;;;; trivial-openstack-test.asd

(asdf:defsystem #:trivial-openstack-test
  :description "trivial-openstack unit tests."
  :author "Smith Dhumbumroong <zodmaner@gmail.com>"
  :license "MIT"
  :depends-on (#:trivial-openstack
               #:fiveam
               #:st-json
               #:uri-template
               #:local-time
               #:hunchentoot)
  :serial t
  :components ((:module "t"
                        :serial t
                        :components
                        ((:module "t.mock-identity-server"
                                  :serial t
                                  :components
                                  ((:file "package")
                                   (:file "mock-identity-server")))
                         (:module "t.mock-image-server"
                                  :serial t
                                  :components
                                  ((:file "package")
                                   (:file "mock-image-server")))
                         (:module "t.mock-compute-server"
                                  :serial t
                                  :components
                                  ((:file "package")
                                   (:file "mock-compute-server")))
                         (:file "package")
                         (:file "trivial-openstack-test")))))
