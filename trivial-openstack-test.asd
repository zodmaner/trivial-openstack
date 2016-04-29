;;;; trivial-openstack-test.asd

(asdf:defsystem #:trivial-openstack-test
  :description "trivial-openstack unit tests."
  :author "Smith Dhumbumroong <zodmaner@gmail.com>"
  :license "MIT"
  :depends-on (#:trivial-openstack
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
                         (:file "package")
                         (:file "trivial-openstack-test")))))
