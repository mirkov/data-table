(in-package :numeric-table)

(defclass linear-least-squares-column-schema (foreign-column-schema)
  ((independent-var
    :accessor independent-var
    :initform nil
    :documentation "Name of column with independent data")
   (fit-coeffs
    :reader fit-coeffs
    :initform nil
    :documentation "List of fitting coefficients")
   (covariance
    :reader covariance
    :initform nil
    :documentation "Three element list storing the covariance matrix
    elements 00 01 and 11")
   (chisq
    :reader chi^2
    :initform nil
    :documentation "chi^2 of the fit"))
  (:documentation "Stores data for linear least squares fitting via GSLL

Also stores the fitting results: coefficients, covariance matrix, and chi^2"))

(defmethod linear-least-squares-fit
    ((table column-major-table) y-name x-name
     &optional weight-name)
  (let ((y-schema (find-column-schema y-name table)))
    (let ((x (table-column x-name y-schema))
	  (y (table-column y-name y-schema))
	  (w (if weight-name
		 (table-column weight-name y-schema)
		 (grid:make-grid `((foreign-array ,(row-count table))
				   double-float)
				 :initial-value 1d0))))
      (multiple-value-bind (c0 c1 cov00 cov01 cov11 chi^2)
	  (gsll:linear-fit x y w)
	(setf (slot-value y-schema 'fit-coeffs) (list c0 c1)
	      (slot-value y-schema 'covariance) (list cov00 cov01 cov11)
	      (slot-value y-schema 'chi^2) chi^2)))))
