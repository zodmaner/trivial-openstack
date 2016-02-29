;;;; compute-api.lisp

(in-package #:trivial-openstack)

;;; Helper functions for retrieving values of response parameters.

(defun cdr-assoc (keyword alist &key (test #'string=))
  (cdr (assoc keyword alist :test test)))

(defun get-parameter (keyword parameters)
  (cdr-assoc keyword parameters))

;;; Bindings for OpenStack Compute API are defined here.

(defgeneric list-flavors (conn))

(defmethod list-flavors ((conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :get "8774" #U/v2.1/{tenant-id}/flavors)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "name" jso)
                        (st-json:getjso "id" jso)))
              (st-json:getjso "flavors" (st-json:read-json response))))))

(defgeneric list-images (conn))

(defmethod list-images ((conn connection))
  (with-connection (response conn :get "9292" "/v2/images")
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (st-json:getjso "id" jso)))
            (st-json:getjso "images" (st-json:read-json response)))))

(defgeneric list-servers (conn))

(defmethod list-servers ((conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :get "8774" #U/v2/{tenant-id}/servers)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "name" jso)
                        (st-json:getjso "id" jso)))
              (st-json:getjso "servers" (st-json:read-json response))))))

(defgeneric list-servers-detail (conn))

(defmethod list-servers-detail ((conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :get "8774" #U/v2/{tenant-id}/servers/detail)
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

(defgeneric create-server (server-name image-id flavor-id conn))

(defmethod create-server (server-name image-id flavor-id (conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :post "8774" #U/v2.1/{tenant-id}/servers
                               (st-json:write-json-to-string
                                (alexandria:plist-hash-table
                                 (list "server"
                                       (alexandria:plist-hash-table
                                        (list "name" server-name
                                              "imageRef" image-id
                                              "flavorRef" flavor-id))))))
      (st-json:getjso "id" (st-json:getjso "server" (st-json:read-json response))))))

(defgeneric delete-server (server-id conn))

(defmethod delete-server (server-id (conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :delete "8774" #U/v2.1/{tenant-id}/servers/{server-id})
      response)))

(defgeneric list-floating-ips (conn))

(defmethod list-floating-ips ((conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :get "8774" #U/v2.1/{tenant-id}/os-floating-ips)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "ip" jso)
                        (pairlis (list "fixed-ip" "pool")
                                 (list (st-json:getjso "fixed_ip" jso)
                                       (st-json:getjso "pool" jso)))))
              (st-json:getjso "floating_ips" (st-json:read-json response))))))

(defgeneric create-floating-ip (conn))

(defmethod create-floating-ip ((conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :post "8774" #U/v2.1/{tenant-id}/os-floating-ips
                               (st-json:write-json-to-string
                                (alexandria:plist-hash-table
                                 (list "pool" "public"))))
      (st-json:getjso "ip" (st-json:getjso "floating_ip" (st-json:read-json response))))))

(defgeneric associate-floating-ip (server-id floating-ip conn))

(defmethod associate-floating-ip (server-id floating-ip (conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :post "8774" #U/v2.1/{tenant-id}/servers/{server-id}/action
                               (st-json:write-json-to-string
                                (alexandria:plist-hash-table
                                 (list "addFloatingIp"
                                       (alexandria:plist-hash-table
                                        (list "address" floating-ip))))))
      response)))

(defgeneric list-default-security-group-rules (conn))

(defmethod list-default-security-group-rules ((conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :get "8774" #U/v2.1/{tenant-id}/os-security-group-default-rules)
      (st-json:read-json response))))

(defgeneric create-default-security-group-rule (rule conn))

(defmethod create-default-security-group-rule (rule (conn connection))
  (let ((tenant-id (tenant-id conn)))
    (with-connection (response conn :post "8774" #U/v2.1/{tenant-id}/os-security-group-default-rules
                               rule)
      response)))

(defun set-security-rule-accept-all-icmp (conn)
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "ICMP"
                                                     "from_port" "-1"
                                                     "to_port" "-1"
                                                     "cidr" "0.0.0.0/0")))))
                                      conn))

(defun set-security-rule-accept-all-tcp (conn)
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "TCP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      conn))

(defun set-security-rule-accept-all-udp (conn)
  (create-default-security-group-rule (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "security_group_default_rule"
                                              (alexandria:plist-hash-table
                                               (list "ip_protocol" "UDP"
                                                     "from_port" "1"
                                                     "to_port" "65535"
                                                     "cidr" "0.0.0.0/0")))))
                                      conn))
