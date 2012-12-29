(in-package :numeric-table)

(export '(linear-least-squares-column-schema
	  linear-fit linear-fit-estimate
	  multiplier-fit multiplier-fit-estimate
	  fit-coeffs
	  covariance
	  chi^2))

(defclass linear-least-squares-column-schema (foreign-column-schema)
  ((independent-var
    :reader independent-var
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
   (chi^2
    :reader chi^2
    :initform nil
    :documentation "chi^2 of the fit"))
  (:documentation "Stores data for linear least squares fitting via GSLL

Also stores the fitting results: coefficients, covariance matrix, and chi^2"))

(defmethod make-column-schema  (name (type (eql 'linear-least-squares-column-schema))
				&key documentation 
				  value-normalizer &allow-other-keys)
  "Makes instance of a non-lin-ls-sq-column-schema.

It replicates the code for foreign-column-schema.  Can I remove this
duplication?"
  (let ((schema (make-instance 'linear-least-squares-column-schema
			       :name name
			       :comparator #'<
			       :equality-predicate #'=
			       :default-type 'number
			       :documentation documentation)))
    (awhen value-normalizer
      (setf (slot-value schema 'value-normalizer) it))
      schema))

(defmethod linear-fit
    ((table column-major-table) y-name x-name
     &optional weight-name)
  (let ((y-schema (find-column-schema y-name table)))
    (let ((x (grid:copy (table-column x-name table)
			:grid-type 'grid:foreign-array
			:element-type 'double-float))
	  (y (grid:copy (table-column y-name table)
			:grid-type 'grid:foreign-array
			:element-type 'double-float))
	  (w (if weight-name
		 (grid:copy (table-column weight-name table)
			    :grid-type 'grid:foreign-array
			    :element-type 'double-float)
		 (grid:make-grid `((grid:foreign-array ,(row-count table))
				   double-float)
				 :initial-element 1d0))))
      (multiple-value-bind (c0 c1 cov00 cov01 cov11 chi^2)
	  (gsll:linear-fit x y w)
	(setf (slot-value y-schema 'fit-coeffs) (list c0 c1)
	      (slot-value y-schema 'covariance) (list cov00 cov01 cov11)
	      (slot-value y-schema 'chi^2) chi^2
	      (slot-value y-schema 'independent-var) x-name)))))

(defmethod linear-fit-estimate ((table column-major-table) y-name x)
  (let* ((y-schema (find-column-schema y-name table))
	 (x-schema (find-column-schema (independent-var y-schema) table)))
    (destructuring-bind (c0 c1)
	(fit-coeffs y-schema)
      (destructuring-bind (cov00 cov01 cov11)
	  (covariance y-schema)
	(gsll:linear-estimate (funcall (value-normalizer x-schema)
				       x x-schema)
			      c0 c1 cov00 cov01 cov11)))))

(defmethod multiplier-fit
    ((table column-major-table) y-name x-name
     &optional weight-name)
  (let ((y-schema (find-column-schema y-name table)))
    (let ((x (grid:copy (table-column x-name table)
			:grid-type 'grid:foreign-array
			:element-type 'double-float))
	  (y (grid:copy (table-column y-name table)
			:grid-type 'grid:foreign-array
			:element-type 'double-float))
	  (w (if weight-name
		 (grid:copy (table-column weight-name table)
			    :grid-type 'grid:foreign-array
			    :element-type 'double-float)
		 (grid:make-grid `((grid:foreign-array ,(row-count table))
				   double-float)
				 :initial-element 1d0))))
      (multiple-value-bind (c1 cov11 chi^2)
	  (gsll:multiplier-fit x y w)
	(setf (slot-value y-schema 'fit-coeffs) c1
	      (slot-value y-schema 'covariance) cov11
	      (slot-value y-schema 'chi^2) chi^2
	      (slot-value y-schema 'independent-var) x-name)))))

(defmethod multiplier-fit-estimate ((table column-major-table)  y-name x)
  (let* ((y-schema (find-column-schema y-name table))
	 (x-schema (find-column-schema (independent-var y-schema) table)))
    (gsll:multiplier-estimate
     (funcall (value-normalizer x-schema)
	      x x-schema)
     (fit-coeffs y-schema) (covariance y-schema))))
