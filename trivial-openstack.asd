;;;; trivial-openstack.asd

(asdf:defsystem #:trivial-openstack
  :description "Describe trivial-openstack here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:st-json
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "trivial-openstack")
               (:file "openstack-compute-api")))

