;;;; image-api.lisp

(in-package #:trivial-openstack)

;;; Bindings for OpenStack Glance image API are defined here.

(defun list-images (&key (conn *connection*))
  "Retrieves the list of currently available images."
  (declare (type connection conn))
  (with-accessors ((endpoints service-endpoints) (token token)) conn
    (with-os-response response
        ((format nil "~A~A"
                 (get-public-url "glance" endpoints) "/v2/images")
         :get token nil)
      (mapcar #'(lambda (jso)
                  (cons (st-json:getjso "name" jso)
                        (st-json:getjso "id" jso)))
              (st-json:getjso "images" (st-json:read-json response))))))
