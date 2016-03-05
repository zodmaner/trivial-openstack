;;;; identity-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Keystone identity API are defined here

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
                :accessor tenant-name))
  (:documentation "A user credential payload, with all the necessary
information to authenticate a user.

Requires a hostname of the Keystone identity service, a username, and
a password.

The tenant-name is an optional value that will default to username if
not provided."))

(defgeneric send-credential-payload (os-c)
  (:documentation "Sends a credential payload to the Keystone identity
service endpoint and returns a stream of response."))

(defmethod send-credential-payload ((os-c os-credential))
  (with-accessors ((hostname keystone-hostname) (username username)
                   (password password) (tenant-name tenant-name)) os-c
    (let ((response
           (send-api-request
            (format nil "http://~A:5000/v2.0/tokens" hostname)
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
  "Makes and returns a new instant of os-credential class."
  (make-instance 'os-credential
                 :keystone-hostname hostname
                 :username username
                 :password password
                 :tenant-name tenant-name))

(defgeneric retrieve-endpoints (os-c)
  (:documentation "Uses the credential object to authenticate a user with the
Keystone service and returns an alist map of currently active service endpoints."))

(defmethod retrieve-endpoints ((os-c os-credential))
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
                     (st-json:getjso "serviceCatalog" token-service-catalog-jso))))
    endpoints))

;;; User's authentication token

(defclass os-auth-token ()
  ((credential :initarg :credential
               :reader credential)
   (token :accessor token)
   (token-expiration-time :accessor token-expiration-time))
  (:documentation "An authentication token returned by the Keystone identity service,
along with all the necessary information to reacquire the token once it expires.

Requires only an instant of user credential payload, other slots will be initialized
when we instantiate the object."))

(defmethod initialize-instance :after ((os-auth-token os-auth-token) &key)
  "Uses an instant of user credential payload to authenticate and retrieve a token and
its expiration time, then store both of them into their respective slots."
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
  "Before reading a value of the token's slot, check if it already expires. If it does,
then uses the credential payload to re-authenticate and reacquire the token."
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
