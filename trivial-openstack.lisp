;;;; trivial-openstack.lisp

(in-package #:trivial-openstack)

;;; "trivial-openstack" goes here. Hacks and glory await!

(defmacro send-api-request (uri http-method &key x-auth-token content)
  "Sends an API request to an OpenStack endpoint at URI and returns a stream
of the response body, NIL if the stream has zero length, or throw an error if
the returned status code is not 200, 202, or 204.

The :x-auth-token keyword can be used to send the authentication token to the
endpoint, while the :content keyword can be used to send any content with the
request."
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
             response-body-stream)
           (error (format nil "Error code ~A, ~A." status-code reason-phase))))))

(defmacro with-api-request (stream (os-auth-token http-method endpoint-uri &optional content)
                            &body body)
  "Sends an API request with an authentication token to an OpenStack endpoint at URI, and binds
a stream of the response body to a specified symbol.

Also, a content, which is usually a JSON object, can be sent along with the request."
  (let ((lambda-list (list :x-auth-token `(token ,os-auth-token))))
    (when content
      (push content lambda-list)
      (push :content lambda-list))
    `(let ((,stream (send-api-request ,endpoint-uri ,http-method ,@lambda-list)))
       ,@body)))
