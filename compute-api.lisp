;;;; compute-api.lisp

(in-package #:trivial-openstack)

;;; Helper functions for retrieving values of response parameters.

(defun cdr-assoc (keyword alist &key (test #'string=))
  "Given a keyword, retrieves a value from an alist."
  (cdr (assoc keyword alist :test test)))

(defun get-public-url (service endpoints)
  "Retrieves a public URL of an OpenStack service from an alist map of currently
active endpoints."
  (cdr-assoc "public-url" (cdr-assoc "endpoints" (cdr-assoc service endpoints))))

;;; Bindings for OpenStack Nova Compute API are defined here.

(defun list-flavors (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Lists all of the currently available flavors."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/flavors")
         :get token nil)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "name" jso)
                        (st-json:getjso "id" jso)))
              (st-json:getjso "flavors" (st-json:read-json response))))))

(defun list-servers (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Lists all of the currently active servers."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/servers")
         :get token nil)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "name" jso)
                        (st-json:getjso "id" jso)))
              (st-json:getjso "servers" (st-json:read-json response))))))

(defun list-servers-detail (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Lists all of the currently active servers in detail."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/servers/detail")
         :get token nil)
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
              (st-json:getjso "servers" (st-json:read-json response))))))

(defun create-server (server-name image-id flavor-id &key (endpoints *endpoints*) (os-auth-token *token*))
  "Creates a new server."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/servers")
         :post token
         (st-json:write-json-to-string
          (alexandria:plist-hash-table
           (list "server"
                 (alexandria:plist-hash-table
                  (list "name" server-name
                        "imageRef" image-id
                        "flavorRef" flavor-id))))))
      (st-json:getjso "id" (st-json:getjso "server" (st-json:read-json response))))))

(defun delete-server (server-id &key (endpoints *endpoints*) (os-auth-token *token*))
  "Deletes a server."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A~A"
                 (get-public-url "nova" endpoints) "/servers/" server-id)
         :delete token nil)
      response)))

(defun list-floating-ips (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Lists all of the currently allocated floating IPs."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/os-floating-ips")
         :get token nil)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "ip" jso)
                        (pairlis (list "fixed-ip" "pool")
                                 (list (st-json:getjso "fixed_ip" jso)
                                       (st-json:getjso "pool" jso)))))
              (st-json:getjso "floating_ips" (st-json:read-json response))))))

(defun create-floating-ip (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Creates/allocates a new floating IP."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/os-floating-ips")
         :post token
         (st-json:write-json-to-string
          (alexandria:plist-hash-table
           (list "pool" "public"))))
      (st-json:getjso "ip" (st-json:getjso "floating_ip" (st-json:read-json response))))))

(defun associate-floating-ip (server-id floating-ip &key (endpoints *endpoints*) (os-auth-token *token*))
  "Associates a floating IP with an active server."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A~A~A"
                 (get-public-url "nova" endpoints) "/servers/"
                 server-id "/action")
         :post token
         (st-json:write-json-to-string
          (alexandria:plist-hash-table
           (list "addFloatingIp"
                 (alexandria:plist-hash-table
                  (list "address" floating-ip))))))
      response)))

(defun list-default-security-group-rules (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Lists all the currently active security rules in the default security group."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints)
                 "/os-security-group-default-rules")
         :get token nil)
      (st-json:read-json response))))

(defun create-default-security-group-rule (rule &key (endpoints *endpoints*) (os-auth-token *token*))
  "Creates a new security rule in the default security group."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints)
                 "/os-security-group-default-rules")
         :post token rule)
      response)))

(defun add-security-rule-accept-all-icmp (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Adds a security rule that accepts all incoming ICMP connection."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "ICMP"
                                                     "from_port" "-1"
                                                     "to_port" "-1"
                                                     "cidr" "0.0.0.0/0")))))
                                      :endpoints endpoints
                                      :os-auth-token os-auth-token))

(defun add-security-rule-accept-all-tcp (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Adds a security rule that accepts all incoming TCP connection."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "TCP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      :endpoints endpoints
                                      :os-auth-token os-auth-token))

(defun add-security-rule-accept-all-udp (&key (endpoints *endpoints*) (os-auth-token *token*))
  "Adds a security rule that accepts all incoming UDP connection."
  (declare (type list endpoints)
           (type os-auth-token os-auth-token))
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "UDP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      :endpoints endpoints
                                      :os-auth-token os-auth-token))
