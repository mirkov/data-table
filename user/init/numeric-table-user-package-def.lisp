(defpackage #:numeric-table-user
  (:nicknames :ntu)
  (:use :cl :lisp-unit :numeric-table)
  (:import-from #:alexandria
		:with-input-from-file)
  (:documentation "User package for package NUMERIC-TABLE"))

(antik:make-user-package "numeric-table-user")
