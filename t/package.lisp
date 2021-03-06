;;;; package.lisp

(defpackage #:trivial-openstack-test
  (:use #:cl #:fiveam #:trivial-openstack)
  (:import-from #:t.mock-identity-server
                #:*token*
                #:start-mock-identity-server
                #:stop-mock-identity-server)
  (:import-from #:t.mock-image-server
                #:start-mock-image-server
                #:stop-mock-image-server)
  (:import-from #:t.mock-compute-server
                #:start-mock-compute-server
                #:stop-mock-compute-server)
  (:export #:run-all-test))
