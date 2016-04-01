;;;; trivial-openstack.lisp

(in-package #:trivial-openstack)

;;; "trivial-openstack" goes here. Hacks and glory await!

(defmacro with-openstack-response (stream (uri http-method &optional x-auth-token content)
                                   &body body)
  "Sends an API request to an OpenStack endpoint at URI and binds a
stream of the response body that is returned by an OpenStack service
to a user specified stream symbol.

If the returned stream has zero length, then NIL will be bound to the
stream symbol. If an error status code is returned, then the function
will throw an error.

The x-auth-token optional argument can be used to send the
authentication token to the endpoint, while the content optional
argument can be used to send any content with the request."
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

(defmacro def-openstack-api (name lambda-list
                             (stream http-method uri-list &optional json auth-token)
                             &body body)
  "Defines a new OpenStack REST API binding."
  (let* ((token-sym (gensym "OS-"))
         (body-car (car body))
         (doc-string (when (stringp body-car)
                       body-car))
         (forms (if doc-string
                    (cdr body)
                    body)))
    `(defun ,name ,lambda-list
       ,doc-string
       (with-accessors ((,token-sym token)) ,(if auth-token
                                                 auth-token
                                                 '*openstack-token*)
         (with-openstack-response ,stream ((apply #'join-strings ,@uri-list '())
                                           ,http-method ,token-sym ,json)
           ,@forms)))))
