(asdf:defsystem #:hunch
  :serial t
  :description "API-only on Hunchentoot"
  :author "mck- <kuomarc2@gmail.com>"
  :license "WTFPL"
  :depends-on (#:hunchentoot #:sb-daemon)
  :components ((:file "package")
               (:file "utils")
               (:file "hunch")
               (:file "routes")))
