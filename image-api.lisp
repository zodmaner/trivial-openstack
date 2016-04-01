;;;; image-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Glance image API are defined here.

(def-openstack-api list-images ()
    (response :get ((get-public-url "glance") "/v2/images"))
  "Retrieves the list of currently available images."
  (mapcar #'(lambda (jso)
              (cons (st-json:getjso "name" jso)
                    (st-json:getjso "id" jso)))
          (st-json:getjso "images" (st-json:read-json response))))
