;;;; utility.lisp

(in-package #:trivial-openstack)

;;; Various utility and helper functions.

(defun join-strings (&rest strings)
  "Joins strings."
  (with-output-to-string (out)
    (loop :for element :in strings :do
       (princ element out))))
