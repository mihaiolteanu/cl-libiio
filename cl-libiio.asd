;;;; cl-libiio.asd

(asdf:defsystem :cl-libiio
  :description "Common Lisp bindings for libiio (https://github.com/analogdevicesinc/libiio)."
  :author "Mihai Olteanu"
  :license "GPLv3"
  :version "0.1"
  :depends-on (:cffi
               :flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "cl-libiio")))
