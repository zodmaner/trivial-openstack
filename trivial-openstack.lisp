;;;; trivial-openstack.lisp

(in-package #:trivial-openstack)

;;; "trivial-openstack" goes here. Hacks and glory await!

;; Binds the default dispatch character for reading URI templates
(uri-template:enable-uri-template-syntax)

(defmacro send-api-request (uri port uri-path http-method &key x-auth-token content)
  (let ((lambda-list (list :method http-method)))
    (when x-auth-token
      (push `(list (cons "X-Auth-Token" ,x-auth-token)) lambda-list)
      (push :additional-headers lambda-list))
    (when content
      (push content lambda-list)
      (push :content lambda-list))
    `(multiple-value-bind (response-body-stream status-code headers uri
                                                stream must-close reason-phase)
         (drakma:http-request (format nil "HTTP://~A:~A~A" ,uri ,port ,uri-path)
                              :content-type "application/json"
                              ,@lambda-list
                              :want-stream t)
       (declare (ignore headers uri stream must-close))
       (if (or (= status-code 200)
               (= status-code 202)
               (= status-code 204))
           (when (not (null (flexi-streams:peek-byte response-body-stream nil nil nil)))
             response-body-stream)
           (error (format nil "Error code ~A, ~A." status-code reason-phase))))))

(defclass connection ()
  ((uri :initarg :uri
        :reader uri)
   (username :initarg :username
             :reader username)
   (password :initarg :password
             :reader password)
   (tenant-name :initarg :tenant-name
                :initform nil
                :accessor tenant-name)
   (tenant-id :initform nil
              :accessor tenant-id)
   (token :initform nil
          :accessor token)
   (token-expiration-time :initform nil
                          :accessor token-expiration-time)))

(defgeneric authenticate (conn))

(defmethod authenticate ((conn connection))
  (let ((response
         (send-api-request
          (uri conn)
          "5000"
          "/v2.0/tokens"
          :post
          :content (st-json:write-json-to-string
                    (alexandria:plist-hash-table
                     (list "auth"
                           (alexandria:plist-hash-table
                            (list "tenantName" (if (null (tenant-name conn))
                                                   (progn
                                                     (setf (tenant-name conn)
                                                           (username conn))
                                                     (tenant-name conn))
                                                   (tenant-name conn))
                                  "passwordCredentials"
                                  (alexandria:plist-hash-table
                                   (list "username" (username conn)
                                         "password" (password conn)))))))))))
    response))

(defun parse-token-object (stream)
  (let ((token-jso
         (st-json:getjso
          "token"
          (st-json:getjso
           "access"
           (st-json:read-json stream)))))
    token-jso))

(defmethod initialize-instance :after ((conn connection) &key)
  (let ((token-jso (parse-token-object (authenticate conn))))
    (setf (token conn) (st-json:getjso "id" token-jso))
    (setf (tenant-id conn)
          (st-json:getjso "id" (st-json:getjso "tenant" token-jso)))
    (setf (token-expiration-time conn)
          (local-time:parse-timestring (st-json:getjso "expires" token-jso)))))

(defmethod token :before ((conn connection))
  (when (local-time:timestamp>= (local-time:now) (token-expiration-time conn))
    (let ((token-jso (parse-token-object (authenticate conn))))
      (setf (token conn) (st-json:getjso "id" token-jso)))))

(defun make-connection (uri username password &optional tenant-name)
  (make-instance 'connection
                 :uri uri
                 :username username
                 :password password
                 :tenant-name tenant-name))

(defmacro with-connection ((stream conn http-method port uri-path &optional content) &body body)
  (let ((lambda-list (list :x-auth-token `(token ,conn))))
    (when content
      (push content lambda-list)
      (push :content lambda-list))
    `(let ((,stream (send-api-request (uri ,conn) ,port ,uri-path ,http-method ,@lambda-list)))
       ,@body)))
