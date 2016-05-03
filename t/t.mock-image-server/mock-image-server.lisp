;;;; mock-image-server.lisp

(in-package #:t.mock-image-server)

;;; OpenStack mock Glance (image) server is defined here.

(defconstant +default-port+ 9292
  "Default port for the test server.")

(defvar *tenant-id* t.mock-identity-server:*tenant-id*
  "A dummy tenant ID (same as the one used in the mock identity
  server.")

(defvar *token* t.mock-identity-server:*token*
  "A dummy token (same as the one used in the mock identity server.")

(defun start-mock-image-server (&key (port +default-port+))
  "Starts the server by starting the easy-acceptor and returns a stop-server
closure function that can be used to stop the active acceptor and shutdown
the server."
  (let ((image-acceptor (hunchentoot:start
                         (make-instance 'hunchentoot:easy-acceptor
                                        :port port))))
    (defun stop-mock-image-server ()
      "A closure function that closes over an active acceptor started by
the start-server function and can be used to stop the said active acceptor
and shutdown the server."
      (hunchentoot:stop image-acceptor))))

(hunchentoot:define-easy-handler
    (glance-endpoint-image :uri "/v2/images") ()
  "A mock OpenStack Glance (image) image endpoint."
  (case (hunchentoot:request-method*)
    (:get
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (st-json:write-json-to-string
        (alexandria:plist-hash-table
         (list "images"
               (list
                (alexandria:plist-hash-table
                 (list "name" "m1.tiny"
                       "id" "1"))
                (alexandria:plist-hash-table
                 (list "name" "m1.small"
                       "id" "2"))
                (alexandria:plist-hash-table
                 (list "name" "m1.medium"
                       "id" "3"))
                (alexandria:plist-hash-table
                 (list "name" "m1.large"
                       "id" "4"))
                (alexandria:plist-hash-table
                 (list "name" "m1.nano"
                       "id" "42"))
                (alexandria:plist-hash-table
                 (list "name" "m1.xlarge"
                       "id" "5"))
                (alexandria:plist-hash-table
                 (list "name" "m1.micro"
                       "id" "84"))))))))))
