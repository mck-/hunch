(asdf:defsystem #:hunch
  :serial t
  :description "API-only on Hunchentoot"
  :author "mck- <kuomarc2@gmail.com>"
  :version "0.2.3"
  :license "WTFPL"
  :depends-on (#:hunchentoot #:sb-daemon #:cl-json #:split-sequence)
  :components ((:file "utils")
               (:file "package")
               (:file "hunch")
               (:file "routes")))
