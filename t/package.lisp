;;;; package.lisp

(defpackage #:trivial-openstack-test
  (:use #:cl #:fiveam #:trivial-openstack)
  (:import-from #:t.mock-identity-server
                #:*token*
                #:start-mock-identity-server
                #:stop-mock-identity-server))
