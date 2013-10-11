(in-package #:hunch)

(defvar *server* nil)
(defvar *default-port* 8688)
(defvar *log-path* #p"server.log")

(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t)

(defun start-api (&optional (port *default-port*))
  (setf *server* (start (make-instance 'easy-acceptor :port port))))

(defun stop-api ()
  (stop *server*))

;;; Entry point for build
(defun main (argv)
  (declare (ignore argv))
  (let ((running t)
        (port (if (cadr argv) (parse-integer (cadr argv)) *default-port*)))
    (format t "Starting API on port ~D~%" port)
    (format t "---=================================---~%")
    (format t "Server log output going to: \"~A\"~%" *log-path*)
    (format t "~%To test if the API is running, curl: http://localhost:~D/~%" port)
    (sb-daemon:daemonize :error *log-path*
                         :exit-parent t
                         :sigterm (lambda (sig)
                                    (declare (ignore sig))
                                    (write-to-log *log-path* "Stopping API daemon ... ")
                                    (stop-api)
                                    (setf running nil)))
    (start-api port)
    (write-to-log *log-path* (format nil "API listening on port ~D" port))
    (loop while running do (sleep 1))))
