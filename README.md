# Trivial-OpenStack

[![Build Status](https://travis-ci.org/zodmaner/trivial-openstack.svg?branch=master)](https://travis-ci.org/zodmaner/trivial-openstack)
[![Coverage Status](https://coveralls.io/repos/github/zodmaner/trivial-openstack/badge.svg?branch=master)](https://coveralls.io/github/zodmaner/trivial-openstack?branch=master)

Trivial-openstack is a simple OpenStack REST client/library
implemented in Common Lisp.

## Installation

Since May 1, 2016, trivial-openstack is available via
[Quicklisp](https://www.quicklisp.org), and can be installed using the
following command:

````lisp
CL-USER> (ql:quickload 'trivial-openstack)
````

## Getting Started

Getting started with trivial-openstack is easy. The first thing we
need to do is to authenticate with the OpenStack Keystone (identity)
service:

````lisp
TRIVIAL-OPENSTACK> (authenticate "192.168.1.8" "admin" "swordfish")
````

And that's it! Now you can invoke any API bindings that you like, for
examples:

````lisp
TRIVIAL-OPENSTACK> (list-images) ; list currently available images
(("cirros-0.3.4-x86_64-uec" . "c4947a88-3b38-44d5-b605-edad3cf1191b")
 ("cirros-0.3.4-x86_64-uec-ramdisk" . "619726e7-b3b1-4d39-8669-cf05fb04981d")
 ("cirros-0.3.4-x86_64-uec-kernel" . "b5afe28f-3ed5-4d4e-8094-fac19d2d7ac3"))
TRIVIAL-OPENSTACK> (list-flavors) ; list currently available flavors
(("m1.tiny" . "1") ("m1.small" . "2") ("m1.medium" . "3")
 ("m1.large" . "4") ("m1.nano" . "42") ("m1.xlarge" . "5")
 ("m1.micro" . "84"))
TRIVIAL-OPENSTACK> (create-server "test-00" "c4947a88-3b38-44d5-b605-edad3cf1191b" "1") ; create a new server
"0a427e44-8d69-4b02-a747-0eb731ba02ad"
TRIVIAL-OPENSTACK> 
````

Expect full documentation in the near future.

## Dependencies

* **drakma**
* **st-json**
* **local-time**
* **alexandria**

## License

Copyright (c) 2016 Smith Dhumbumroong

Licensed under the MIT License.
