;;;; identity-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Keystone identity API are defined here

;;; Helper functions

(defun parse-endpoints (service-catalog-jso)
  "Parses a JSON containing currently active service endpoints."
  (mapcar
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
   service-catalog-jso))

(defun get-public-url (service endpoints)
  "Retrieves a public URL of an OpenStack service from an alist map of
currently active endpoints."
  (alexandria:assoc-value
   (alexandria:assoc-value
    (alexandria:assoc-value endpoints service :test #'string=)
    "endpoints" :test #'string=)
   "public-url" :test #'string=))

;;; User's OpenStack credential payload

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
  (:documentation "An OpenStack credential payload, with all the
necessary information to authenticate a user.

Requires a hostname of the Keystone identity service, a username, and
a password. The tenant-name is an optional argument that will default
to username if not provided."))

(defmacro with-keystone-response (stream (os-c) &body body)
  "Authenticates a user by sending a credential payload to the
Keystone identity service endpoint and binds a stream of the response
that is returned to a specified stream symbol."
  `(with-os-response
       ,stream ((format nil "http://~A:5000/v2.0/tokens" (keystone-hostname ,os-c))
                :post
                nil
                (st-json:write-json-to-string
                 (alexandria:plist-hash-table
                  (list "auth"
                        (alexandria:plist-hash-table
                         (list "tenantName" (if (null (tenant-name ,os-c))
                                                (progn
                                                  (setf (tenant-name ,os-c) (username ,os-c))
                                                  (tenant-name ,os-c))
                                                (tenant-name ,os-c))
                               "passwordCredentials"
                               (alexandria:plist-hash-table
                                (list "username" (username ,os-c)
                                      "password" (password ,os-c)))))))))
     ,@body))

(defun make-credential (hostname username password &optional tenant-name)
  "Makes and returns a new instant of os-credential class."
  (make-instance 'os-credential
                 :keystone-hostname hostname
                 :username username
                 :password password
                 :tenant-name tenant-name))

(defclass connection ()
  ((credential :initarg :credential
               :reader credential)
   (service-endpoints :accessor service-endpoints)
   (token :accessor token)
   (token-expiration-time :accessor token-expiration-time))
  (:documentation "A connection object containing the currently active
service endpoints as well as an authentication token along with all
the necessary information to reacquire the token once it expires.

Only requires an instant of user credential payload, other slots will
be initialized when we instantiate the object."))

(defmethod initialize-instance :after ((conn connection) &key)
  "Uses an instant of user credential payload to authenticate and
retrieve the currently active service endpoints, a token and its
expiration time, then store three of them into their respective
slots."
  (with-accessors ((os-c credential) (endpoints service-endpoints) (token token)
                   (token-expiration-time token-expiration-time)) conn
    (with-keystone-response response (os-c)
      (let* ((access-jso (st-json:getjso "access" (st-json:read-json response)))
             (token-jso (st-json:getjso "token" access-jso))
             (service-catalog-jso (st-json:getjso "serviceCatalog" access-jso)))
        (setf endpoints (parse-endpoints service-catalog-jso))
        (setf token (st-json:getjso "id" token-jso))
        (setf token-expiration-time
              (local-time:parse-timestring (st-json:getjso "expires" token-jso)))))))

(defmethod token :before ((conn connection))
  "Before reading a value of the token's slot, check if it has already
expired. If it does, then uses the credential payload to
re-authenticate and reacquire the token."
  (with-accessors ((os-c credential) (token token)
                   (token-expiration-time token-expiration-time)) conn
    (when (local-time:timestamp>= (local-time:now) token-expiration-time)
      (with-keystone-response response (os-c)
        (let ((token-jso (st-json:getjso
                          "token"
                          (st-json:getjso
                           "access"
                           (st-json:read-json
                            response)))))
          (setf token (st-json:getjso "id" token-jso))
          (setf token-expiration-time
                (local-time:parse-timestring (st-json:getjso "expires" token-jso))))))))

(defun make-connection (keystone-hostname username password &optional tenant-name)
  "Creates and returns a new instant of the connection object."
  (let ((os-c (make-instance 'os-credential
                             :keystone-hostname keystone-hostname
                             :username username
                             :password password
                             :tenant-name tenant-name)))
    (make-instance 'connection :credential os-c)))
