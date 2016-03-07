;;;; package.lisp

(defpackage #:trivial-openstack
  (:use #:cl)
  (:nicknames #:t-os)
  (:export :cdr-assoc
           :get-public-url
           :authenticate
           :*endpoints*
           :*token*
           :define-global-endpoints-and-token
           :list-images
           :list-flavors
           :list-servers
           :list-servers-detail
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

