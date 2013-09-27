;;;; Routes -- define your routes and handlers here

(in-package #:hunch)

(setq *dispatch-table*
      (list
       (create-regex-dispatcher "^/$" 'index)
       (create-regex-dispatcher "^/echo$" 'echo)))

(defun index ()
  (log-message* :info "TEST ------ ")
  (with-json-response
    (format nil "Hello World")))

(defun echo ()
  (with-json-response
    (format nil "~S" (post-body))))
