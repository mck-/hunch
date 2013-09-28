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

;;; JSON/Alist utils

(defun aval (key alist)
  "Given alist and key, return value"
  (cdr (assoc key alist :test #'equal)))

(defun val-reversed (alist &rest keys)
  "Given an alist, and a list of keys, retrieve value dot-notation style (reversed)"
  (if (null keys)
      alist
      (aval (first keys) (apply #'val-reversed alist (rest keys)))))

(defun val (alist &rest keys)
  "Given an alist, and a list of keys, retrieve value dot-notation style."
  (apply #'val-reversed alist (reverse keys)))

(defmacro with-json-to-alist ((var) &body body)
  "Converts raw JSON to Alist and binds to var"
  (let ((stream (gensym)))
    `(with-input-from-string (,stream (post-body))
       (let ((,var (decode-json ,stream)))
         ,@body))))
