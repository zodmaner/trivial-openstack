;;;; mock-identity-server.lisp

(in-package #:t.mock-identity-server)

;;; OpenStack mock Keystone (identity) server is defined here.

(defvar *tenant-id* "4869da5d1b38f29b4a9f6333972e48db"
  "A dummy tenant ID.")

(defvar *token* "182be908ba792497d186dbaa1d01361b"
  "A dummy token.")

(defun make-token-expiration-time ()
  (local-time:format-rfc3339-timestring
   t
   (local-time:timestamp+ (local-time:now) 1 :hour)))

(defun start-mock-identity-server (&key (port 5000))
  "Starts the server by starting the easy-acceptor and returns a stop-server
closure function that can be used to stop the active acceptor and shutdown
the server."
  (let ((identity-acceptor (hunchentoot:start
                            (make-instance 'hunchentoot:easy-acceptor
                                           :port port))))
    (defun stop-mock-identity-server ()
      "A closure function that closes over an active acceptor started by
the start-server function and can be used to stop the said active acceptor
and shutdown the server."
      (hunchentoot:stop identity-acceptor))))

(hunchentoot:define-easy-handler
    (keystone-endpoint :uri "/v2.0/tokens"
                       :default-request-type :post) ()
  "A mock OpenStack Keystone (identity) endpoint."
  (case (hunchentoot:request-method*)
    (:post
     (let* ((request-jso (st-json:read-json-from-string
                          (flexi-streams:octets-to-string
                           (hunchentoot:raw-post-data))))
            (auth-jso (st-json:getjso "auth" request-jso))
            (tenant-name (st-json:getjso "tenantName" auth-jso))
            (pwd-creds-jso (st-json:getjso "passwordCredentials" auth-jso))
            (username (st-json:getjso "username" pwd-creds-jso))
            (password (st-json:getjso "password" pwd-creds-jso)))
       (when (and (string= username "dummy") (string= password "swordfish")
                  (string= tenant-name "dummy"))
         (st-json:write-json-to-string
          (plist-hash-table
           (list "access"
                 (plist-hash-table
                  (list "token"
                        (plist-hash-table
                         (list "id" *token* "expires" (make-token-expiration-time)))
                        "serviceCatalog"
                        (list
                         (plist-hash-table
                          (list "name" "nova" "type" "compute"
                                "endpoints" (list (plist-hash-table
                                                   (list "publicURL" #Uhttp://localhost:8774/v2.1/{*tenant-id*}
                                                         "adminURL" #Uhttp://localhost:8774/v2.1/{*tenant-id*}
                                                         "region" "RegionOne")))))
                         (plist-hash-table
                          (list "name" "glance" "type" "image"
                                "endpoints" (list (plist-hash-table
                                                   (list "publicURL" #Uhttp://localhost:9292
                                                         "adminURL" #Uhttp://localhost:9292
                                                         "region" "RegionOne")))))
                         (plist-hash-table
                          (list "name" "keystone" "type" "identity"
                                "endpoints" (list (plist-hash-table
                                                   (list "publicURL" #Uhttp://localhost:5000/v2.0
                                                         "adminURL" #Uhttp://localhost:5000/v2.0
                                                         "region" "RegionOne"))))))))))))))))
