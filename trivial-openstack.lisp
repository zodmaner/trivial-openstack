;;;; trivial-openstack.lisp

(in-package #:trivial-openstack)

;;; "trivial-openstack" goes here. Hacks and glory await!

(defvar *endpoints* nil
  "A global default alist map of active OpenStack service endpoints.")

(defvar *token* nil
  "A global default authentication token returned by OpenStack Keystone identity service.")

(defmacro with-os-response (stream (uri http-method &optional x-auth-token content) &body body)
  "Sends an API request to an OpenStack endpoint at URI and binds a stream of
the response body that is returned by an OpenStack service to a user specified
stream symbol.

If the returned stream has zero length, then NIL will be bound to the stream
symbol. If an error status code is returned, then the function will throw an
error.

The x-auth-token optional argument can be used to send the authentication token
to the endpoint, while the content optional argument can be used to send any
content with the request."
  (let ((lambda-list (list :method http-method)))
    (when x-auth-token
      (push `(list (cons "X-Auth-Token" ,x-auth-token)) lambda-list)
      (push :additional-headers lambda-list))
    (when content
      (push content lambda-list)
      (push :content lambda-list))
    `(multiple-value-bind (response-body-stream status-code headers uri
                                                stream must-close reason-phase)
         (drakma:http-request ,uri
                              :content-type "application/json"
                              ,@lambda-list
                              :want-stream t)
       (declare (ignore headers uri stream must-close))
       (if (or (= status-code 200)
               (= status-code 202)
               (= status-code 204))
           (when (not (null (flexi-streams:peek-byte response-body-stream nil nil nil)))
             (let ((,stream response-body-stream))
               ,@body))
           (error (format nil "Error code ~A, ~A." status-code reason-phase))))))

(defun define-global-endpoints-and-token (keystone-hostname username password &optional tenant-name)
  "Authenticates a user, retrieves and sets new global default endpoints and authentication token."
  (multiple-value-bind (endpoints token)
      (authenticate keystone-hostname username password tenant-name)
    (setf *endpoints* endpoints)
    (setf *token* token)))
