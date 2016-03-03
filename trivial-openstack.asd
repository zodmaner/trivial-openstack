;;;; trivial-openstack.asd

(asdf:defsystem #:trivial-openstack
  :description "A simple Common Lisp OpenStack REST client."
  :author "Smith Dhumbumroong <zodmaner@gmail.com>"
  :license "LGPL"
  :depends-on (#:drakma
               #:st-json
               #:uri-template
               #:local-time
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "trivial-openstack")
               (:file "identity-api")
               (:file "image-api")
               (:file "compute-api")))

