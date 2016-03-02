;;;; image-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Glance image API are defined here.

(defgeneric list-images (endpoints os-auth-token))

(defmethod list-images (endpoints (os-auth-token os-auth-token))
  (with-api-request response
      (os-auth-token :get (format nil "~A~A"
                                  (get-public-url "glance" endpoints) "/v2/images"))
    (mapcar #'(lambda (jso)
                (cons (st-json:getjso "name" jso)
                      (st-json:getjso "id" jso)))
            (st-json:getjso "images" (st-json:read-json response)))))
