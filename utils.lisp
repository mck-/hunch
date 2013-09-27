;;;; Utilities

(in-package #:hunch)

(defmacro with-json-response (&body body)
  "Set JSON response headers"
  `(progn
     (setf (header-out "Access-Control-Allow-Origin") "*")
     (setf (header-out "Content-Type") "application/json;charset=UTF-8")
     (setf (header-out "Cache-Control") "no-cache, must-revalidate")
     ,@body))

(defun write-to-log (string)
  "Append string to log file in *log-path*"
  (with-open-file (stream *log-path* :direction :output :if-exists :append)
    (format stream (format nil "~&~a~%" string))))

(defun post-body ()
  "Returns the raw post data form request. To be used in routes."
  (raw-post-data :force-text t))
