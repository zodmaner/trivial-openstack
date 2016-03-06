;;;; image-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Glance image API are defined here.

(defgeneric list-images (endpoints os-auth-token)
  (:documentation "Retrieves the list of currently available images."))

(defmethod list-images (endpoints (os-auth-token os-auth-token))
  (with-accessors ((token token)) os-auth-token
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "glance" endpoints) "/v2/images")
         :get token nil)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "name" jso)
                        (st-json:getjso "id" jso)))
              (st-json:getjso "images" (st-json:read-json response))))))
