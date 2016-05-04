;;;; package.lisp

(defpackage #:trivial-openstack
  (:use #:cl)
  (:export :with-openstack-response
           :def-openstack-api
           ;; Keystone API bindings
           :*service-catalog*
           :get-public-url
           :*openstack-token*
           :token
           :token-expiration-time
           :authenticate
           ;; Glance API bindings
           :list-images
           ;; Nova API bindings
           :list-flavors
           :list-flavor-details
           :list-servers
           :list-servers-details
           :create-server
           :delete-server
           :list-floating-ips
           :create-floating-ip
           :associate-floating-ip
           :list-default-security-group-rules
           :create-default-security-group-rule
           :add-security-rule-accept-all-icmp
           :add-security-rule-accept-all-tcp
           :add-security-rule-accept-all-udp))

