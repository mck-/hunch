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
  (defparameter *running* t)
  (sb-daemon:daemonize :error *log-path*
                       :exit-parent t
                       :sigterm (lambda (sig)
                                  (declare (ignore sig))
                                  (write-to-log "Stopping API daemon ... ")
                                  (stop-api)
                                  (setf *running* nil)))
  (setf *running* t)
  (start-api)
  (write-to-log (format nil "API listening on port ~D" *default-port*))
  (loop while *running* do (sleep 1)))
