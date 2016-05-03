;;;; mock-image-server.lisp

(in-package #:t.mock-image-server)

;;; OpenStack mock Glance (image) server is defined here.

(defconstant +default-port+ 9292
  "Default port for the test server.")

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
                 (list "name" "cirros-0.3.4-x86_64-uec"
                       "id" "c4947a88-3b38-44d5-b605-edad3cf1191b"))
                (alexandria:plist-hash-table
                 (list "name" "cirros-0.3.4-x86_64-uec-ramdisk"
                       "id" "619726e7-b3b1-4d39-8669-cf05fb04981d"))
                (alexandria:plist-hash-table
                 (list "name" "cirros-0.3.4-x86_64-uec-kernel"
                       "id" "b5afe28f-3ed5-4d4e-8094-fac19d2d7ac3"))))))))))
