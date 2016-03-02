;;;; compute-api.lisp

(in-package #:trivial-openstack)

;;; Helper functions for retrieving values of response parameters.

(defun cdr-assoc (keyword alist &key (test #'string=))
  (cdr (assoc keyword alist :test test)))

(defun get-parameter (keyword parameters)
  (cdr-assoc keyword parameters))

(defun get-public-url (service endpoints)
  "Retrieves a public URL of an OpenStack service."
  (cdr-assoc "public-url" (cdr-assoc "endpoints" (cdr-assoc service endpoints))))

;;; Bindings for OpenStack Nova Compute API are defined here.

(defgeneric list-flavors (endpoints os-auth-token))

(defmethod list-flavors (endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :get (format nil "~A~A"
                                  (get-public-url "nova" endpoints) "/flavors"))
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (st-json:getjso "id" jso)))
            (st-json:getjso "flavors" (st-json:read-json response)))))

(defgeneric list-servers (endpoints os-auth-token))

(defmethod list-servers (endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :get (format nil "~A~A"
                                  (get-public-url "nova" endpoints) "/servers"))
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (st-json:getjso "id" jso)))
            (st-json:getjso "servers" (st-json:read-json response)))))

(defgeneric list-servers-detail (endpoints os-auth-token))

(defmethod list-servers-detail (endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :get (format nil "~A~A"
                                  (get-public-url "nova" endpoints) "/servers/detail"))
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (pairlis (list "id" "status" "addresses")
                               (list
                                (st-json:getjso "id" jso)
                                (st-json:getjso "status" jso)
                                (mapcar #'(lambda (jso)
                                            (cons (st-json:getjso "OS-EXT-IPS:type" jso)
                                                  (st-json:getjso "addr" jso)))
                                        (st-json:getjso
                                         "private"
                                         (st-json:getjso
                                          "addresses"
                                          jso)))))))
            (st-json:getjso "servers" (st-json:read-json response)))))

(defgeneric create-server (server-name image-id flavor-id endpoints os-auth-token))

(defmethod create-server (server-name image-id flavor-id endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :post (format nil "~A~A"
                                   (get-public-url "nova" endpoints) "/servers")
                     (st-json:write-json-to-string
                      (alexandria:plist-hash-table
                       (list "server"
                             (alexandria:plist-hash-table
                              (list "name" server-name
                                    "imageRef" image-id
                                    "flavorRef" flavor-id))))))
    (st-json:getjso "id" (st-json:getjso "server" (st-json:read-json response)))))

(defgeneric delete-server (server-id endpoints os-auth-token))

(defmethod delete-server (server-id endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :delete (format nil "~A~A~A"
                                     (get-public-url "nova" endpoints) "/servers/" server-id))
    response))

(defgeneric list-floating-ips (endpoints os-auth-token))

(defmethod list-floating-ips (endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :get (format nil "~A~A"
                                  (get-public-url "nova" endpoints) "/os-floating-ips"))
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "ip" jso)
                      (pairlis (list "fixed-ip" "pool")
                               (list (st-json:getjso "fixed_ip" jso)
                                     (st-json:getjso "pool" jso)))))
            (st-json:getjso "floating_ips" (st-json:read-json response)))))

(defgeneric create-floating-ip (endpoints os-auth-token))

(defmethod create-floating-ip (endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :post (format nil "~A~A"
                                   (get-public-url "nova" endpoints) "/os-floating-ips")
                     (st-json:write-json-to-string
                      (alexandria:plist-hash-table
                       (list "pool" "public"))))
    (st-json:getjso "ip" (st-json:getjso "floating_ip" (st-json:read-json response)))))

(defgeneric associate-floating-ip (server-id floating-ip endpoints os-auth-token))

(defmethod associate-floating-ip (server-id floating-ip endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :post (format nil "~A~A~A~A"
                                   (get-public-url "nova" endpoints) "/servers/"
                                   server-id "/action")
                     (st-json:write-json-to-string
                      (alexandria:plist-hash-table
                       (list "addFloatingIp"
                             (alexandria:plist-hash-table
                              (list "address" floating-ip))))))
    response))

(defgeneric list-default-security-group-rules (endpoints os-auth-token))

(defmethod list-default-security-group-rules (endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :get (format nil "~A~A"
                                  (get-public-url "nova" endpoints)
                                  "/os-security-group-default-rules"))
    (st-json:read-json response)))

(defgeneric create-default-security-group-rule (rule endpoints os-auth-token))

(defmethod create-default-security-group-rule (rule endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :post (format nil "~A~A"
                                   (get-public-url "nova" endpoints)
                                   "/os-security-group-default-rules")
                     rule)
    response))

(defun set-security-rule-accept-all-icmp (endpoints os-auth-token)
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "ICMP"
                                                     "from_port" "-1"
                                                     "to_port" "-1"
                                                     "cidr" "0.0.0.0/0")))))
                                      endpoints
                                      os-auth-token))

(defun set-security-rule-accept-all-tcp (endpoints os-auth-token)
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "TCP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      endpoints
                                      os-auth-token))

(defun set-security-rule-accept-all-udp (endpoints os-auth-token)
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "UDP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      endpoints
                                      os-auth-token))
