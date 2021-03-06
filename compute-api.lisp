;;;; compute-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Nova Compute API are defined here.

(def-openstack-api list-flavors ()
    (response :get ((get-public-url "nova") "/flavors"))
  "Lists all of the currently available flavors."
  (mapcar #'(lambda (jso)
              (cons (st-json:getjso "name" jso)
                    (st-json:getjso "id" jso)))
          (st-json:getjso "flavors" (st-json:read-json response))))

(def-openstack-api list-flavor-details (flavor-id)
    (response :get ((get-public-url "nova") "/flavors/" flavor-id))
  "List a flavor details."
  (let ((jso (st-json:read-json response)))
    (pairlis (list "name" "vcpus" "ram" "swap" "disk")
             (list (st-json:getjso* "flavor.name" jso)
                   (st-json:getjso* "flavor.vcpus" jso)
                   (st-json:getjso* "flavor.ram" jso)
                   (st-json:getjso* "flavor.swap" jso)
                   (st-json:getjso* "flavor.disk" jso)))))

(def-openstack-api list-servers ()
    (response :get ((get-public-url "nova") "/servers"))
  "Lists all of the currently active servers."
  (mapcar #'(lambda (jso)
              (cons (st-json:getjso "name" jso)
                    (st-json:getjso "id" jso)))
          (st-json:getjso "servers" (st-json:read-json response))))

(def-openstack-api list-servers-details ()
    (response :get ((get-public-url "nova") "/servers/detail"))
  "Lists all of the currently active servers in details."
  (mapcar #'(lambda (jso)
              (cons (st-json:getjso "name" jso)
                    (pairlis (list "id" "status" "addresses")
                             (list
                              (st-json:getjso "id" jso)
                              (st-json:getjso "status" jso)
                              (mapcar #'(lambda (jso)
                                          (cons (st-json:getjso "OS-EXT-IPS:type" jso)
                                                (st-json:getjso "addr" jso)))
                                      (st-json:getjso* "addresses.private" jso))))))
          (st-json:getjso "servers" (st-json:read-json response))))

(def-openstack-api create-server (server-name image-id flavor-id)
    (response :post ((get-public-url "nova") "/servers")
              (st-json:write-json-to-string
               (plist-hash-table
                (list "server"
                      (plist-hash-table
                       (list "name" server-name
                             "imageRef" image-id
                             "flavorRef" flavor-id))))))
  "Creates a new server."
  (st-json:getjso* "server.id" (st-json:read-json response)))

(def-openstack-api delete-server (server-id)
    (response :delete ((get-public-url "nova") "/servers/" server-id))
  "Deletes a server."
  response)

(def-openstack-api list-floating-ips ()
    (response :get ((get-public-url "nova") "/os-floating-ips"))
  "Lists all of the currently allocated floating IPs."
  (mapcar #'(lambda (jso)
              (cons (st-json:getjso "ip" jso)
                    (pairlis (list "fixed-ip" "pool")
                             (list (st-json:getjso "fixed_ip" jso)
                                   (st-json:getjso "pool" jso)))))
          (st-json:getjso "floating_ips" (st-json:read-json response))))

(def-openstack-api create-floating-ip (&key (pool "public"))
    (response :post ((get-public-url "nova") "/os-floating-ips")
              (st-json:write-json-to-string
               (plist-hash-table
                (list "pool" pool))))
  "Creates/allocates a new floating IP."
  (st-json:getjso* "floating_ip.ip" (st-json:read-json response)))

(def-openstack-api associate-floating-ip (server-id floating-ip)
    (response :post ((get-public-url "nova") "/servers/" server-id "/action")
              (st-json:write-json-to-string
               (plist-hash-table
                (list "addFloatingIp"
                      (plist-hash-table
                       (list "address" floating-ip))))))
  "Associates a floating IP with an active server."
  response)

(def-openstack-api list-default-security-group-rules ()
    (response :get ((get-public-url "nova") "/os-security-group-default-rules"))
  "Lists all the currently active security rules in the default security group."
  (st-json:read-json response))

(def-openstack-api create-default-security-group-rule (rule)
    (response :post ((get-public-url "nova") "/os-security-group-default-rules")
              rule)
  "Creates a new security rule in the default security group."
  response)

(defun add-security-rule-accept-all-icmp ()
  "Adds a security rule that accepts all incoming ICMP connection to
the default security group."
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (plist-hash-table
                                        (list "security_group_default_rule"
                                              (plist-hash-table
                                               (list "ip_protocol" "ICMP"
                                                     "from_port" "-1"
                                                     "to_port" "-1"
                                                     "cidr" "0.0.0.0/0")))))))

(defun add-security-rule-accept-all-tcp ()
  "Adds a security rule that accepts all incoming TCP connection to
the default security group."
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (plist-hash-table
                                        (list "security_group_default_rule"
                                              (plist-hash-table
                                               (list "ip_protocol" "TCP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))))

(defun add-security-rule-accept-all-udp ()
  "Adds a security rule that accepts all incoming UDP connection to
the default security group."
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (plist-hash-table
                                        (list "security_group_default_rule"
                                              (plist-hash-table
                                               (list "ip_protocol" "UDP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))))
