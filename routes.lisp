;;;; Routes -- define your routes and handlers here

(in-package #:hunch)

(define-easy-handler (index :uri "/index" :default-request-type :get) ()
  (log-message* :info "GET on index ------ ")
  (with-json-response
    "Hello World"))

(define-easy-handler (echo :uri "/echo" :default-request-type :post) ()
  (log-message* :info "POST on echo ------ ")
  (with-json-response
    (post-body)))
