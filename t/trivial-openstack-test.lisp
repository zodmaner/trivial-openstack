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
         (when (and (not (null *token*))
                    (not (null *service-catalog*)))
           (setf *token* nil)
           (setf *service-catalog* nil))
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

(test images-api
  "Test the images REST API."
  (with-fixture mock-image-server ()
    (is (equalp (list-images)
                '(("cirros-0.3.4-x86_64-uec" . "c4947a88-3b38-44d5-b605-edad3cf1191b")
                  ("cirros-0.3.4-x86_64-uec-ramdisk" . "619726e7-b3b1-4d39-8669-cf05fb04981d")
                  ("cirros-0.3.4-x86_64-uec-kernel" . "b5afe28f-3ed5-4d4e-8094-fac19d2d7ac3"))))))

(test flavors-api
  "Test the flavors REST API."
  (with-fixture mock-compute-server ()
    (is (equalp (list-flavors)
                '(("m1.tiny" . "1") ("m1.small" . "2") ("m1.medium" . "3")
                  ("m1.large" . "4") ("m1.nano" . "42") ("m1.xlarge" . "5")
                  ("m1.micro" . "84"))))
    (is (equalp (list-flavor-details "1")
                '(("disk" . 1) ("swap" . "")
                  ("ram" . 512) ("vcpus" . 1) ("name" . "m1.tiny"))))))
