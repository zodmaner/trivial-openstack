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

(hunchentoot:define-easy-handler
    (nova-endpoint-flavor-details
     :uri #'(lambda (request)
              (uri-template:uri-template-bind
                  (#U/v2.1/{tenant-id}/flavors/{id})
                  (hunchentoot:request-uri request)
                (and (string= tenant-id *tenant-id*)
                     (string= id "1"))))) ()
  "A list flavor details endpoint."
  (case (hunchentoot:request-method*)
    (:get
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (st-json:write-json-to-string
        (alexandria:plist-hash-table
         (list "flavor"
               (alexandria:plist-hash-table
                (list "name" "m1.tiny"
                      "vcpus" 1
                      "ram" 512
                      "swap" ""
                      "disk" 1)))))))))

(hunchentoot:define-easy-handler
    (nova-endpoint-servers :uri #U/v2.1/{*tenant-id*}/servers) ()
  "An endpoint for server API."
  (case (hunchentoot:request-method*)
    (:post
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (let* ((request-jso (st-json:read-json-from-string
                            (flexi-streams:octets-to-string
                             (hunchentoot:raw-post-data))))
              (server-name (st-json:getjso* "server.name" request-jso))
              (image-ref (st-json:getjso* "server.imageRef" request-jso))
              (flavor-ref (st-json:getjso* "server.flavorRef" request-jso)))
         (when (and (stringp server-name)
                    (string= "c4947a88-3b38-44d5-b605-edad3cf1191b" image-ref)
                    (string= "1" flavor-ref))
           (st-json:write-json-to-string
            (alexandria:plist-hash-table
             (list "server"
                   (alexandria:plist-hash-table
                    (list "id" "0a427e44-8d69-4b02-a747-0eb731ba02ad")))))))))
    (:get
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (st-json:write-json-to-string
        (alexandria:plist-hash-table
         (list "servers"
               (list
                (alexandria:plist-hash-table
                 (list "name" "test-00"
                       "id" "0a427e44-8d69-4b02-a747-0eb731ba02ad"))))))))))

(hunchentoot:define-easy-handler
    (nova-endpoint-servers-details-api :uri #U/v2.1/{*tenant-id*}/servers/detail) ()
  "An endpoint for servers details API."
  (case (hunchentoot:request-method*)
    (:get
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (st-json:write-json-to-string
        (alexandria:plist-hash-table
         (list "servers"
               (list
                (alexandria:plist-hash-table
                 (list "name" "test-00"
                       "id" "0a427e44-8d69-4b02-a747-0eb731ba02ad"
                       "status" "ACTIVE"
                       "addresses"                      
                       (alexandria:plist-hash-table
                        (list "private"
                              (list
                               (alexandria:plist-hash-table
                                (list "addr" "10.0.0.2"
                                      "OS-EXT-IPS:type" "fixed")))))))))))))))

(hunchentoot:define-easy-handler
    (nova-endpoint-delete-server-api
     :uri #'(lambda (request)
              (uri-template:uri-template-bind
                  (#U/v2.1/{tenant-id}/servers/{server-id})
                  (hunchentoot:request-uri request)
                (and (string= tenant-id *tenant-id*)
                     (string= server-id "0a427e44-8d69-4b02-a747-0eb731ba02ad"))))) ()
  "An endpoint for deleting a server API."
  (case (hunchentoot:request-method*)
    (:delete
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))))))

(hunchentoot:define-easy-handler
    (nova-endpoint-floating-ip :uri #U/v2.1/{*tenant-id*}/os-floating-ips) ()
  "An endpoint for floating IP API."
  (case (hunchentoot:request-method*)
    (:post
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (let* ((request-jso (st-json:read-json-from-string
                            (flexi-streams:octets-to-string
                             (hunchentoot:raw-post-data))))
              (pool (st-json:getjso "pool" request-jso)))
         (when (string= "public" pool)
           (st-json:write-json-to-string
            (alexandria:plist-hash-table
             (list "floating_ip"
                   (alexandria:plist-hash-table
                    (list "ip" "192.168.1.225")))))))))
    (:get
     (when (string= *token* (hunchentoot:header-in* "X-Auth-Token"))
       (st-json:write-json-to-string
        (alexandria:plist-hash-table
         (list "floating_ips"
               (list
                (alexandria:plist-hash-table
                 (list "ip" "192.168.1.225"
                       "fixed-ip" :null
                       "pool" "public"))))))))))