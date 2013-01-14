(asdf:defsystem #:numeric-table-user
  :serial t
  :description "Example of a numeric-table user system"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :license "MIT"
  :depends-on ("lisp-unit"
	       "alexandria"
	       "numeric-table"
	       "anaphora"
	       "antik"
	       "gsll")
  :components
  ((:module "init"
	    :pathname #p"./user/init/"
	    :serial t
	    :components
	    ((:file "numeric-table-user-package-def")))
   (:module "example1"
	    :pathname #p"./user/example1/"
	    :serial t
	    :components
	    ((:file "example-w-native-arrays")
	     (:file "example-w-foreign-arrays")
	     (:file "example-w-interpolation")
	     (:file "example-w-fitting")
	     (:file "example-w-fft")))))
