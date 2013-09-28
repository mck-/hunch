;;;; Routes -- define your routes and handlers here

(in-package #:hunch)

(define-easy-handler (index :uri "/index" :default-request-type :get) ()
  (log-message* :info "GET on index ------ ")
  (with-json-response
    (format nil "Hello World")))

(define-easy-handler (echo :uri "/echo" :default-request-type :post) ()
  (log-message* :info "POST on index ------ ")
  (with-json-response
    (format nil "~S" (post-body))))
