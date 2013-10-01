;;;; Utilities

(defpackage #:hunch.util
  (:use #:cl #:hunchentoot #:cl-json #:split-sequence)
  (:export :with-json-response
           :write-to-log
           :val
           :with-json-to-alist))

(in-package #:hunch.util)

(defmacro with-json-response (&body body)
  "Set JSON response headers"
  `(progn
     (setf (header-out "Access-Control-Allow-Origin") "*")
     (setf (header-out "Content-Type") "application/json;charset=UTF-8")
     (setf (header-out "Cache-Control") "no-cache, must-revalidate")
     ,@body))

(defun write-to-log (file string)
  "Append string to log file in *log-path*"
  (with-open-file (stream file :direction :output :if-exists :append)
    (format stream (format nil "~&~a~%" string))))

(defun post-body ()
  "Returns the raw post data form request. To be used in routes."
  (raw-post-data :force-text t))

;;; JSON/Alist utils

(defun aval (key alist)
  "Given alist and key, return value -- symbols and strings are interchangable for keys"
  (cdr (assoc key alist :test #'(lambda (x y) (string= (string x) (string y))))))

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


;; Expands: (dot-accessor obj.key.key2)
;; into:    (val obj "key" "key2")
(defmacro dot-accessor (input)
  "JSON dot-style accessor for Alists
   usage: @obj.key.key2 to access obj = { key: { key2: true } }"
  (let ((keys (split-sequence #\. (string input))))
    `(val ,(intern (car keys)) ,@(cdr keys))))

(set-macro-character #\@ #'(lambda (stream char)
                             (declare (ignore char))
                             (list 'dot-accessor (read stream t nil t))))
