;;;; Routes -- define your routes and handlers here

(in-package #:hunch)

(define-easy-handler (index :uri "/" :default-request-type :get) ()
  (log-message* :info "GET on index ------ ")
  (with-json-response
    (encode-json-to-string
     {:http-code "200"
     :status "API is running!"})))

(define-easy-handler (echo :uri "/echo" :default-request-type :post) ()
  (log-message* :info "POST on echo ------ ")
  (with-json-response
    (post-body)))

(define-easy-handler (name :uri "/name" :default-request-type :post) ()
  (with-json-to-alist (body)
    (log-message* :info "~S" (print body))
    (log-message* :info "POST on name with { name: ~a, age: ~d }" @body.name @body.age)
    (with-json-response
      (format nil "Hello ~A, it seems like your are turning ~d soon?" @body.name (1+ @body.age)))))
