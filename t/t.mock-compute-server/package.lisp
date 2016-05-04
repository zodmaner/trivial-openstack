;;;; t.mock-compute-server module's package.lisp

(defpackage #:t.mock-compute-server
  (:use #:cl)
  (:import-from #:t.mock-identity-server
                #:*tenant-id*
                #:*token*)
  (:export #:start-mock-compute-server
           #:stop-mock-compute-server))
