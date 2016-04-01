;;;; identity-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Keystone identity API are defined here

(defvar *service-catalog* nil
  "An alist map of service catalog endpoints, the value of which will
  be set when the OpenStack token object is initialized.")

(defvar *openstack-token* nil
  "The default global OpenStack token object.")

(defun get-public-url (service &key (endpoints *service-catalog*))
  "Retrieves a public URL of an OpenStack service endpoint from an
alist map of currently active endpoints."
  (alexandria:assoc-value
   (alexandria:assoc-value
    (alexandria:assoc-value endpoints service :test #'string=)
    "endpoints" :test #'string=)
   "public-url" :test #'string=))

(defun parse-endpoints (service-catalog-jso)
  "Parses a JSON containing currently active service endpoints into an
alist map."
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

(defmacro with-keystone-response (stream (os-c) &body body)
  "Authenticates a user by sending a credential payload to the
Keystone identity service endpoint and binds a stream of the response
that is returned to a specified stream symbol."
  `(with-openstack-response
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

;;; User's OpenStack credential payload object

(defclass openstack-credential ()
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

(defun make-openstack-credential (hostname username password &optional tenant-name)
  "Makes and returns a new instant of OpenStack credential class."
  (make-instance 'openstack-credential
                 :keystone-hostname hostname
                 :username username
                 :password password
                 :tenant-name tenant-name))

;;; OpenStack authentication token object

(defclass openstack-token ()
  ((credential :initarg :credential
               :reader credential)
   (token :accessor token)
   (token-expiration-time :accessor token-expiration-time))
  (:documentation "An OpenStack token object containing an
authentication token along with all the necessary information to
reacquire the token once it expires.

Only requires an instant of user credential payload, other slots will
be initialized when we instantiate the object."))

(defmethod initialize-instance :after ((os-token openstack-token) &key)
  "Uses an instant of user credential payload to authenticate and
retrieve a token and its expiration time, then stores them into their
respective slots.

Also retrieves currently active service catalog endpoints, parses them
into an alist map, and binds the alist map to the global special
variable."
  (with-accessors ((os-c credential) (token token)
                   (token-expiration-time token-expiration-time)) os-token
    (with-keystone-response response (os-c)
      (let* ((access-jso (st-json:getjso "access" (st-json:read-json response)))
             (token-jso (st-json:getjso "token" access-jso))
             (service-catalog-jso (st-json:getjso "serviceCatalog" access-jso)))
        (setf token (st-json:getjso "id" token-jso))
        (setf token-expiration-time
              (local-time:parse-timestring (st-json:getjso "expires" token-jso)))
        (setf *service-catalog* (parse-endpoints service-catalog-jso))))))

(defmethod token :before ((os-token openstack-token))
  "Before reading a value of the token's slot, check if it has already
expired. If it does, then uses the credential payload to
re-authenticate and reacquire the token."
  (with-accessors ((os-c credential) (token token)
                   (token-expiration-time token-expiration-time)) os-token
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

(defun make-openstack-token (keystone-hostname username password &optional tenant-name)
  "Creates and returns a new instant of the OpenStack token object."
  (let ((os-c (make-openstack-credential keystone-hostname
                                         username
                                         password
                                         tenant-name)))
    (make-instance 'openstack-token :credential os-c)))

(defun authenticate (keystone-hostname username password &optional tenant-name)
  "Authenticates a user, and initializes the default global special
variables."
  (setf *openstack-token*
        (make-openstack-token keystone-hostname username password tenant-name)))
