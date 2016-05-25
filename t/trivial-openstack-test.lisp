;;;; trivial-openstack-test.lisp

(in-package #:trivial-openstack-test)

;;; trivial-openstack-test goes here.

(def-suite openstack-test-suite
    :description "OpenStack test suite.")

(in-suite openstack-test-suite)

(def-fixture mock-server (service)
  (unwind-protect
       (progn
         (case service
           (:identity
            (start-mock-identity-server)
            (when (not (null *openstack-keystone*))
              (setf *openstack-keystone* nil)))
           (:image
            (start-mock-image-server))
           (:compute
            (start-mock-compute-server)))
         (&body))
    (case service
      (:identity
       (stop-mock-identity-server))
      (:image
       (stop-mock-image-server))
      (:compute
       (stop-mock-compute-server)))))

(test authentication
  "Test the authentication functionality."
  (with-fixture mock-server (:identity)
    (is-true (authenticate "localhost" "dummy" "swordfish"))
    (is (equalp (service-catalog *openstack-keystone*)
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
    (is (string= (token *openstack-keystone*)
                 *token*))))

(test image-api
  "Test the image API."
  (with-fixture mock-server (:image)
    (is (equalp (list-images)
                '(("cirros-0.3.4-x86_64-uec" . "c4947a88-3b38-44d5-b605-edad3cf1191b")
                  ("cirros-0.3.4-x86_64-uec-ramdisk" . "619726e7-b3b1-4d39-8669-cf05fb04981d")
                  ("cirros-0.3.4-x86_64-uec-kernel" . "b5afe28f-3ed5-4d4e-8094-fac19d2d7ac3"))))))

(test flavor-api
  "Test the flavor API."
  (with-fixture mock-server (:compute)
    (is (equalp (list-flavors)
                '(("m1.tiny" . "1") ("m1.small" . "2") ("m1.medium" . "3")
                  ("m1.large" . "4") ("m1.nano" . "42") ("m1.xlarge" . "5")
                  ("m1.micro" . "84"))))
    (is (equalp (list-flavor-details "1")
                '(("disk" . 1) ("swap" . "")
                  ("ram" . 512) ("vcpus" . 1) ("name" . "m1.tiny"))))))

(test server-api
  "Test the server API."
  (with-fixture mock-server (:compute)
    (is (equalp (create-server "test-00" "c4947a88-3b38-44d5-b605-edad3cf1191b" "1")
                "0a427e44-8d69-4b02-a747-0eb731ba02ad"))
    (is (equalp (list-servers)
                '(("test-00" . "0a427e44-8d69-4b02-a747-0eb731ba02ad"))))
    (is (equalp (list-servers-details)
                '(("test-00" ("addresses" ("fixed" . "10.0.0.2")) ("status" . "ACTIVE")
                   ("id" . "0a427e44-8d69-4b02-a747-0eb731ba02ad")))))
    (is-false (delete-server "0a427e44-8d69-4b02-a747-0eb731ba02ad"))))

(test floating-ip
  "Test the floating IP API."
  (with-fixture mock-server (:compute)
    (is (string= (create-floating-ip)
                 "192.168.1.225"))
    (is (equalp (list-floating-ips)
                '(("192.168.1.225" ("pool" . "public") ("fixed-ip" . :NULL)))))
    (is-false (associate-floating-ip "0a427e44-8d69-4b02-a747-0eb731ba02ad"
                                     "192.168.1.225"))))
