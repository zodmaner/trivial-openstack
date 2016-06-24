;;;; trivial-openstack.lisp

(in-package #:trivial-openstack)

;;; "trivial-openstack" goes here. Hacks and glory await!

(defun join-strings (&rest strings)
  "Joins strings."
  (with-output-to-string (out)
    (loop :for element :in strings :do
       (princ element out))))

(defun get-value (alist &rest keys)
  "Retrieves the value of a given key in an alist.

If multiple keys are supplied, then get-value will recursively descend
into the nested alist and retrieve the value of the last (or the
rightmost) key.

Note that supplying multiple keys only makes sense when the alist has
other alists nested inside."
  (if (cdr keys)
      (apply #'get-value
             (assoc-value alist (car keys) :test #'string=)
             (cdr keys))
      (assoc-value alist (car keys) :test #'string=)))

(defmacro with-openstack-response (response-stream (uri http-method &optional x-auth-token content)
                                   &body body)
  "Sends an API request to an OpenStack endpoint at URI and binds a
stream of the response body that is returned by an OpenStack service
to a user specified response stream symbol.

If the returned stream has zero length, then NIL will be bound to the
stream symbol. If an error status code is returned, then the function
will throw an error.

The x-auth-token optional argument can be used to send the
authentication token to the endpoint, while the content optional
argument can be used to send additional content (usually a JSON) along
with the request."
  (labels ((gen-http-request-args (args x c)
             (cond
               ((null args) (cond
                              (x (cons :additional-headers
                                       (cons `(list (cons "X-Auth-Token" ,x))
                                             (gen-http-request-args args nil c))))
                              (c (cons :content
                                       (cons c
                                             (gen-http-request-args args x nil))))
                              (t '())))
               (t (cons (car args)
                        (gen-http-request-args (cdr args) x c))))))
    (let ((response-body-stream (gensym "W-OS-"))
          (status-code (gensym "W-OS-"))
          (headers (gensym "W-OS-"))
          (reply-uri (gensym "W-OS-"))
          (stream (gensym "W-OS-"))
          (must-close (gensym "W-OS-"))
          (reason-phase (gensym "W-OS-")))
      `(multiple-value-bind (,response-body-stream
                             ,status-code
                             ,headers
                             ,reply-uri
                             ,stream
                             ,must-close
                             ,reason-phase)
           (drakma:http-request ,@(gen-http-request-args (list uri
                                                               :want-stream t
                                                               :content-type "application/json"
                                                               :method http-method)
                                                         x-auth-token
                                                         content))
         (declare (ignore ,headers ,reply-uri ,stream ,must-close))
         (if (or (= ,status-code 200)
                 (= ,status-code 202)
                 (= ,status-code 204))
             (when (not (null (flexi-streams:peek-byte ,response-body-stream nil nil nil)))
               (let ((,response-stream ,response-body-stream))
                 ,@body))
             (error (format nil "Error code ~A, ~A." ,status-code ,reason-phase)))))))

(defmacro def-openstack-api (name lambda-list
                             (stream http-method uri-list &optional json os-keystone)
                             &body body)
  "Defines a new OpenStack REST API binding."
  (let* ((keystone-object (if os-keystone
                              os-keystone
                              '*openstack-keystone*))
         (token (gensym "OS-TOKEN-")))
    (multiple-value-bind (forms doc-string declaration)
        ((lambda (body)
           (cond
             ((listp (car body))
              (if (eql 'declare (caar body))
                  (if (stringp (cadr body))
                      (values (cddr body) (cadr body) (car body))
                      (values (cdr body) nil (car body)))
                  body))
             ((stringp (car body))
              (values (cdr body) (car body)))))
         body)
      `(defun ,name ,lambda-list
         ,@(if declaration
               (list declaration doc-string)
               (list doc-string))
         (with-accessors ((,token token)) ,keystone-object
           (with-openstack-response ,stream ((apply #'join-strings ,@uri-list '())
                                             ,http-method ,token ,json)
             ,@forms))))))
