;;;; t.mock-image-server module's package.lisp

(defpackage #:t.mock-image-server
  (:use #:cl
        #:alexandria)
  (:import-from #:t.mock-identity-server
                #:*tenant-id*
                #:*token*)
  (:export #:start-mock-image-server
           #:stop-mock-image-server))
