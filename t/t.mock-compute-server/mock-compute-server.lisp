;;;; mock-compute-server.lisp

(in-package #:t.mock-compute-server)

;;; OpenStack mock Nova (compute) server is defined here.

(defconstant +default-port+ 8774)

(defun start-mock-compute-server (&key (port +default-port+))
  "Starts the server by starting the easy-acceptor and returns a
stop-server closure function that can be used to stop the active
acceptor and shutdown the server."
  (let ((compute-acceptor (hunchentoot:start
                           (make-instance 'hunchentoot:easy-acceptor
                                          :port port))))
    (defun stop-mock-compute-server ()
      "A closure function that closes over an active acceptor started
by the start-server function and can be used to stop the said active
acceptor and shutdown the server."
      (hunchentoot:stop compute-acceptor))))

(hunchentoot:define-easy-handler
    (nova-endpoint-flavor :uri #U/v2.1/{*tenant-id*}/flavors) ()
  "A mock OpenStack Nova (compute) flavor endpoint."
  (case (hunchentoot:request-method*)
    (:get
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (st-json:write-json-to-string
        (alexandria:plist-hash-table
         (list "flavors"
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
