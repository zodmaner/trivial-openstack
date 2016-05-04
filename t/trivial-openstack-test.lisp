;;;; trivial-openstack-test.lisp

(in-package #:trivial-openstack-test)

;;; trivial-openstack-test goes here.

(def-suite openstack-test-suite
    :description "OpenStack test suite.")

(in-suite openstack-test-suite)

(def-fixture mock-identity-server ()
  (unwind-protect
       (progn
         (start-mock-identity-server)
         (&body))
    (stop-mock-identity-server)))

(def-fixture mock-image-server ()
  (unwind-protect
       (progn
         (start-mock-image-server)
         (&body))
    (stop-mock-image-server)))

(def-fixture mock-compute-server ()
  (unwind-protect
       (progn
         (start-mock-compute-server)
         (&body))
    (stop-mock-compute-server)))

(test authentication
  "Test the authentication functionality."
  (with-fixture mock-identity-server ()
    (is-true (authenticate "localhost" "dummy" "swordfish"))
    (is (equalp *service-catalog*
                '(("nova"
                   ("endpoints"
                    ("public-url" . "http://localhost:8774/v2.1/4869da5d1b38f29b4a9f6333972e48db")
                    ("region" . "RegionOne")
                    ("admin-url" . "http://localhost:8774/v2.1/4869da5d1b38f29b4a9f6333972e48db"))
                   ("type" . "compute"))
                  ("glance"
                   ("endpoints"
                    ("public-url" . "http://localhost:9292")
                    ("region" . "RegionOne")
                    ("admin-url" . "http://localhost:9292"))
                   ("type" . "image"))
                  ("keystone"
                   ("endpoints"
                    ("public-url" . "http://localhost:5000/v2.0")
                    ("region" . "RegionOne")
                    ("admin-url" . "http://localhost:5000/v2.0"))
                   ("type" . "identity")))))
    (is (string= (token *openstack-token*)
                 *token*))))
