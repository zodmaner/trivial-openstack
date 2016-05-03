;;;; t.mock-identity-server module's package.lisp

(defpackage #:t.mock-identity-server
  (:use #:cl)
  (:export #:*tenant-id*
           #:*token*
           #:start-mock-identity-server
           #:stop-mock-identity-server))

(uri-template:enable-uri-template-syntax)
