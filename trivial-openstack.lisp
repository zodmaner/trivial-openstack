;;;; trivial-openstack.lisp

(in-package #:trivial-openstack)

;;; "trivial-openstack" goes here. Hacks and glory await!

(defun cdr-assoc (keyword alist &key (test #'string=))
  (cdr (assoc keyword alist :test test)))

(defmacro send-api-request (uri port uri-path http-method &key x-auth-token content)
  (let ((lambda-list (list :method http-method)))
    (when x-auth-token
      (push `(list (cons "X-Auth-Token" ,x-auth-token)) lambda-list)
      (push :additional-headers lambda-list))
    (when content
      (push content lambda-list)
      (push :content lambda-list))
    `(multiple-value-bind (response-body-stream status-code headers uri
                                                stream must-close reason-phase)
         (drakma:http-request (format nil "HTTP://~A:~A~A" ,uri ,port ,uri-path)
                              :content-type "application/json"
                              ,@lambda-list
                              :want-stream t)
       (declare (ignore headers uri stream must-close))
       (if (or (= status-code 200)
               (= status-code 202)
               (= status-code 204))
           (when (not (null (flexi-streams:peek-byte response-body-stream nil nil nil)))
             response-body-stream)
           (error (format nil "Error code ~A, ~A." status-code reason-phase))))))

(defclass connection ()
  ((uri :initarg :uri
        :reader connection-uri)
   (username :initarg :username
             :reader connection-username)
   (password :initarg :password
             :reader connection-password)
   (tenant-name :initarg :tenant-name
                :initform nil
                :accessor connection-tenant-name)
   (tenant-id :initform nil
              :accessor connection-tenant-id)
   (token :initform nil
          :accessor connection-token)
   (token-creation-time :initform nil
                        :accessor connection-token-creation-time)
   (token-expiration-time :initform nil
                          :accessor connection-token-expiration-time)))

(defgeneric authenticate (conn))

(defmethod authenticate ((conn connection))
  (let ((response
         (send-api-request
          (connection-uri conn)
          "5000"
          "/v2.0/tokens"
          :post
          :content (st-json:write-json-to-string
                    (alexandria:plist-hash-table
                     (list "auth"
                           (alexandria:plist-hash-table
                            (list "tenantName" (if (null (connection-tenant-name conn))
                                                   (progn
                                                     (setf (connection-tenant-name conn)
                                                           (connection-username conn))
                                                     (connection-tenant-name conn))
                                                   (connection-tenant-name conn))
                                  "passwordCredentials"
                                  (alexandria:plist-hash-table
                                   (list "username" (connection-username conn)
                                         "password" (connection-password conn)))))))))))
    response))

(defun parse-token-object (stream)
  (let ((token-jso
         (st-json:getjso
          "token"
          (st-json:getjso
           "access"
           (st-json:read-json stream)))))
    token-jso))

(defmethod initialize-instance :after ((conn connection) &key)
  (let ((token-jso (parse-token-object (authenticate conn))))
    (setf (connection-token conn) (st-json:getjso "id" token-jso))
    (setf (connection-tenant-id conn)
          (st-json:getjso "id" (st-json:getjso "tenant" token-jso)))
    (setf (connection-token-creation-time conn) (get-universal-time))
    (setf (connection-token-expiration-time conn) (st-json:getjso "expires" token-jso))))

(defmethod connection-token :before ((conn connection))
  (when (<= 3000 (- (get-universal-time) (connection-token-creation-time conn)))
    (let ((token-jso (parse-token-object (authenticate conn))))
      (setf (connection-token conn) (st-json:getjso "id" token-jso))
      (setf (connection-token-creation-time conn) (get-universal-time)))))

(defun make-connection (uri username password &optional tenant-name)
  (make-instance 'connection
                 :uri uri
                 :username username
                 :password password
                 :tenant-name tenant-name))

(defmacro with-connection ((stream conn http-method port uri-path &optional content) &body body)
  (let ((lambda-list (list :x-auth-token `(connection-token ,conn))))
    (when content
      (push content lambda-list)
      (push :content lambda-list))
    `(let ((,stream (send-api-request (connection-uri ,conn)
                                      ,port
                                      ,uri-path
                                      ,http-method
                                      ,@lambda-list)))
       ,@body)))

(defgeneric list-flavors (conn))

(defmethod list-flavors ((conn connection))
  (with-connection (response conn :get "8774" (format nil
                                                      "/v2.1/~A/flavors"
                                                      (connection-tenant-id conn)))
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (st-json:getjso "id" jso)))
            (st-json:getjso "flavors" (st-json:read-json response)))))

(defgeneric list-images (conn))

(defmethod list-images ((conn connection))
  (with-connection (response conn :get "9292" "/v2/images")
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (st-json:getjso "id" jso)))
            (st-json:getjso "images" (st-json:read-json response)))))

(defgeneric list-servers (conn))

(defmethod list-servers ((conn connection))
  (with-connection (response conn :get "8774" (format nil
                                                      "/v2/~A/servers"
                                                      (connection-tenant-id conn)))
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (st-json:getjso "id" jso)))
            (st-json:getjso "servers" (st-json:read-json response)))))

(defgeneric list-servers-detail (conn))

(defmethod list-servers-detail ((conn connection))
  (with-connection (response conn :get "8774" (format nil
                                                      "/v2/~A/servers/detail"
                                                      (connection-tenant-id conn)))
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

(defgeneric create-server (server-name image-id flavor-id conn))

(defmethod create-server (server-name image-id flavor-id (conn connection))
  (with-connection (response conn :post "8774" (format nil
                                                       "/v2.1/~A/servers"
                                                       (connection-tenant-id conn))
                             (st-json:write-json-to-string
                              (alexandria:plist-hash-table
                               (list "server"
                                     (alexandria:plist-hash-table
                                      (list "name" server-name
                                            "imageRef" image-id
                                            "flavorRef" flavor-id))))))
    (st-json:getjso "id" (st-json:getjso "server" (st-json:read-json response)))))

(defgeneric delete-server (server-id conn))

(defmethod delete-server (server-id (conn connection))
  (with-connection (response conn :delete "8774" (format nil
                                                         "/v2.1/~A/servers/~A"
                                                         (connection-tenant-id conn)
                                                         server-id))
    response))

(defgeneric list-floating-ips (conn))

(defmethod list-floating-ips ((conn connection))
  (with-connection (response conn :get "8774" (format nil
                                                      "/v2.1/~A/os-floating-ips"
                                                      (connection-tenant-id conn)))
    (mapcar #'(lambda (jso)
                (pairlis (list "ip" "fixed-ip" "pool")
                         (list (st-json:getjso "ip" jso)
                               (st-json:getjso "fixed_ip" jso)
                               (st-json:getjso "pool" jso))))
            (st-json:getjso "floating_ips" (st-json:read-json response)))))

(defgeneric create-floating-ip (conn))

(defmethod create-floating-ip ((conn connection))
  (with-connection (response conn :post "8774" (format nil
                                                       "/v2.1/~A/os-floating-ips"
                                                       (connection-tenant-id conn))
                             (st-json:write-json-to-string
                              (alexandria:plist-hash-table
                               (list "pool" "public"))))
    (st-json:getjso "ip" (st-json:getjso "floating_ip" (st-json:read-json response)))))

(defgeneric associate-floating-ip (server-id floating-ip conn))

(defmethod associate-floating-ip (server-id floating-ip (conn connection))
  (with-connection (response conn :post "8774" (format nil
                                                       "/v2.1/~A/servers/~A/action"
                                                       (connection-tenant-id conn)
                                                       server-id)
                             (st-json:write-json-to-string
                              (alexandria:plist-hash-table
                               (list "addFloatingIp"
                                     (alexandria:plist-hash-table
                                      (list "address" floating-ip))))))
    response))

(defgeneric list-default-security-group-rules (conn))

(defmethod list-default-security-group-rules ((conn connection))
  (with-connection (response conn :get "8774" (format nil
                                                      "/v2.1/~A/os-security-group-default-rules"
                                                      (connection-tenant-id conn)))
    (st-json:read-json response)))

(defgeneric create-default-security-group-rule (rule conn))

(defmethod create-default-security-group-rule (rule (conn connection))
  (with-connection (response conn :post "8774" (format nil
                                                       "/v2.1/~A/os-security-group-default-rules"
                                                       (connection-tenant-id conn))
                             rule)
    response))

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
