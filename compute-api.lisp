;;;; compute-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Nova Compute API are defined here.

(defun list-flavors (&key (conn *connection*))
  "Lists all of the currently available flavors."
  (declare (type connection conn))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/flavors")
         :get token nil)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "name" jso)
                        (st-json:getjso "id" jso)))
              (st-json:getjso "flavors" (st-json:read-json response))))))

(defun list-flavor-details (flavor-id &key (conn *connection*))
  "List details of a flavor."
  (declare (type connection conn)
           (type string flavor-id))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (with-os-response response
        ((format nil "~A~A~A"
                 (get-public-url "nova" endpoints) "/flavors/" flavor-id)
         :get token nil)
      (let ((flavor-jso (st-json:getjso "flavor" (st-json:read-json response))))
        (pairlis (list "name" "vcpus" "ram" "swap" "disk")
                 (list (st-json:getjso "name" flavor-jso)
                       (st-json:getjso "vcpus" flavor-jso)
                       (st-json:getjso "ram" flavor-jso)
                       (st-json:getjso "swap" flavor-jso)
                       (st-json:getjso "disk" flavor-jso)))))))

(defun list-servers (&key detail (conn *connection*))
  "Lists all of the currently active servers."
  (declare (type connection conn)
           (type boolean detail))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (if (null detail)
        (with-os-response response
            ((format nil "~A~A"
                     (get-public-url "nova" endpoints) "/servers")
             :get token nil)
          (mapcar #'(lambda (jso)
                      (cons (st-json:getjso "name" jso)
                            (st-json:getjso "id" jso)))
                  (st-json:getjso "servers" (st-json:read-json response))))
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
                  (st-json:getjso "servers" (st-json:read-json response)))))))

(defun create-server (server-name image-id flavor-id &key (conn *connection*))
  "Creates a new server."
  (declare (type connection conn)
           (type string server-name image-id flavor-id))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
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

(defun delete-server (server-id &key (conn *connection*))
  "Deletes a server."
  (declare (type connection conn)
           (type string server-id))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (with-os-response response
        ((format nil "~A~A~A"
                 (get-public-url "nova" endpoints) "/servers/" server-id)
         :delete token nil)
      response)))

(defun list-floating-ips (&key (conn *connection*))
  "Lists all of the currently allocated floating IPs."
  (declare (type connection conn))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
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

(defun create-floating-ip (&key (conn *connection*))
  "Creates/allocates a new floating IP."
  (declare (type connection conn))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints) "/os-floating-ips")
         :post token
         (st-json:write-json-to-string
          (alexandria:plist-hash-table
           (list "pool" "public"))))
      (st-json:getjso "ip" (st-json:getjso "floating_ip" (st-json:read-json response))))))

(defun associate-floating-ip (server-id floating-ip &key (conn *connection*))
  "Associates a floating IP with an active server."
  (declare (type connection conn)
           (type string server-id floating-ip))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
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

(defun list-default-security-group-rules (&key (conn *connection*))
  "Lists all the currently active security rules in the default security group."
  (declare (type connection conn))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints)
                 "/os-security-group-default-rules")
         :get token nil)
      (st-json:read-json response))))

(defun create-default-security-group-rule (rule &key (conn *connection*))
  "Creates a new security rule in the default security group."
  (declare (type connection conn)
           (type string rule))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "nova" endpoints)
                 "/os-security-group-default-rules")
         :post token rule)
      response)))

(defun add-security-rule-accept-all-icmp (&key (conn *connection*))
  "Adds a security rule that accepts all incoming ICMP connection."
  (declare (type connection conn))
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "ICMP"
                                                     "from_port" "-1"
                                                     "to_port" "-1"
                                                     "cidr" "0.0.0.0/0")))))
                                      :conn conn))

(defun add-security-rule-accept-all-tcp (&key (conn *connection*))
  "Adds a security rule that accepts all incoming TCP connection."
  (declare (type connection conn))
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "TCP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      :conn conn))

(defun add-security-rule-accept-all-udp (&key (conn *connection*))
  "Adds a security rule that accepts all incoming UDP connection."
  (declare (type connection conn))
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "UDP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      :conn conn))
