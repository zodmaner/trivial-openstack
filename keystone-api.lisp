;;;; keystone-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Keystone REST API are defined here

;;; User's credential

(defclass os-credential ()
  ((keystone-hostname :initarg :keystone-hostname
                      :reader keystone-hostname)
   (username :initarg :username
             :reader username)
   (password :initarg :password
             :reader password)
   (tenant-name :initarg :tenant-name
                :initform nil
                :accessor tenant-name)))

(defgeneric send-credential-payload (os-c))

(defmethod send-credential-payload ((os-c os-credential))
  (with-accessors ((hostname keystone-hostname) (username username)
                   (password password) (tenant-name tenant-name)) os-c
    (let ((response
           (send-api-request
            #Uhttp://{hostname}:5000/v2.0/tokens
            :post
            :content (st-json:write-json-to-string
                      (alexandria:plist-hash-table
                       (list "auth"
                             (alexandria:plist-hash-table
                              (list "tenantName" (if (null tenant-name)
                                                     (progn
                                                       (setf tenant-name username)
                                                       tenant-name)
                                                     tenant-name)
                                    "passwordCredentials"
                                    (alexandria:plist-hash-table
                                     (list "username" username
                                           "password" password))))))))))
      response)))

(defun make-credential (hostname username password &optional tenant-name)
  (make-instance 'os-credential
                 :keystone-hostname hostname
                 :username username
                 :password password
                 :tenant-name tenant-name))

(defgeneric authenticate (os-c))

(defmethod authenticate ((os-c os-credential))
  (let* ((response (send-credential-payload os-c))
         (token-service-catalog-jso
          (st-json:getjso "access" (st-json:read-json response)))
         (endpoints (mapcar
                     #'(lambda (jso)
                         (cons (st-json:getjso "name" jso)
                               (pairlis (list "type" "endpoints")
                                        (list
                                         (st-json:getjso "type" jso)
                                         (let ((endpoints
                                                (car (st-json:getjso "endpoints" jso))))
                                           (pairlis
                                            (list "admin-url" "region" "public-url")
                                            (list
                                             (st-json:getjso "adminURL" endpoints)
                                             (st-json:getjso "region" endpoints)
                                             (st-json:getjso "publicURL" endpoints))))))))
                     (st-json:getjso "serviceCatalog" token-service-catalog-jso)))
         (auth-token (make-instance 'os-auth-token :credential os-c)))
    (values auth-token endpoints)))

;;; User's authentication token

(defclass os-auth-token ()
  ((credential :initarg :credential
               :reader credential)
   (token :accessor token)
   (token-expiration-time :accessor token-expiration-time)))

(defmethod initialize-instance :after ((os-auth-token os-auth-token) &key)
  (with-accessors ((os-c credential) (token token)
                   (token-expiration-time token-expiration-time)) os-auth-token
    (let ((token-jso (st-json:getjso
                      "token"
                      (st-json:getjso
                       "access"
                       (st-json:read-json
                        (send-credential-payload os-c))))))
      (setf token (st-json:getjso "id" token-jso))
      (setf token-expiration-time
            (local-time:parse-timestring (st-json:getjso "expires" token-jso))))))

(defmethod token :before ((os-auth-token os-auth-token))
  (with-accessors ((os-c credential) (token token)
                   (token-expiration-time token-expiration-time)) os-auth-token
    (when (local-time:timestamp>= (local-time:now) token-expiration-time)
      (let ((token-jso (st-json:getjso
                        "token"
                        (st-json:getjso
                         "access"
                         (st-json:read-json
                          (send-credential-payload os-c))))))
        (setf token (st-json:getjso "id" token-jso))
        (setf token-expiration-time
              (local-time:parse-timestring (st-json:getjso "expires" token-jso)))))))
