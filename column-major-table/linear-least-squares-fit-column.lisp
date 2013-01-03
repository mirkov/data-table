(in-package :numeric-table)

(export '(ax+b-ls-fit ax-ls-fit
	  linear-fit linear-fit-estimate
	  multiplier-fit multiplier-fit-estimate
	  fit-coeffs
	  covariance
	  chi^2))

(defclass ax+b-least-squares-column-schema (linear-column-fit foreign-double-schema)
  ()
  (:documentation "Stores data for ax+b least squares fitting via GSLL

Also stores the fitting results: coefficients, covariance matrix, and chi^2"))

(add-column-schema-short+long-names 'ax+b-ls-fit 'ax+b-least-squares-column-schema)


(defclass ax-least-squares-column-schema (linear-column-fit foreign-double-schema)
  ()
  (:documentation "Stores data for ax least squares fitting via GSLL

Also stores the fitting results: coefficients, covariance matrix, and chi^2"))

(add-column-schema-short+long-names 'ax-ls-fit 'ax-least-squares-column-schema)


(defmethod fit-column
    ((table column-major-table) (y-schema ax+b-least-squares-column-schema)
     &key)
  (let ((y-name (column-name y-schema))
	(x-name (col-independent-var y-schema))
	(w-spec (sigma y-schema)))
    (let ((x (table-column x-name table))
	  (y (table-column y-name table))
	  (w (if (symbolp w-spec)
		 (table-column w-spec table)
		 (grid:make-grid `((grid:foreign-array ,(row-count table))
				   double-float)
				 :initial-element 1d0))))
      (multiple-value-bind (c0 c1 cov00 cov01 cov11 chi^2)
	  (gsll:linear-fit x y w)
	(setf (slot-value y-schema 'fit-coeffs) (list c0 c1)
	      (slot-value y-schema 'covariance) (list cov00 cov01 cov11)
	      (slot-value y-schema 'chi^2) chi^2
	      (slot-value y-schema 'independent-var) x-name)))))

(defmethod fit-estimate ((table column-major-table)
			 (y-schema ax+b-least-squares-column-schema) x-value)
  (let* ((x-schema (find-column-schema (independent-var y-schema) table)))
    (destructuring-bind (c0 c1)
	(fit-coeffs y-schema)
      (destructuring-bind (cov00 cov01 cov11)
	  (covariance y-schema)
	(gsll:linear-estimate (funcall (value-normalizer x-schema)
				       x-value x-schema)
			      c0 c1 cov00 cov01 cov11)))))

(defmethod fit-column
    ((table column-major-table) (y-schema ax-least-squares-column-schema)
     &key)
  (let ((y-name (column-name y-schema))
	(x-name (col-independent-var y-schema))
	(w-spec (sigma y-schema)))
    (let ((x (table-column x-name table))
	  (y (table-column y-name table))
	  (w (if (symbolp w-spec)
		 (table-column w-spec table) 
		 (grid:make-grid `((grid:foreign-array ,(row-count table))
				   double-float)
				 :initial-element 1d0))))
      (multiple-value-bind (c1 cov11 chi^2)
	  (gsll:multiplier-fit x y w)
	(setf (slot-value y-schema 'fit-coeffs) c1
	      (slot-value y-schema 'covariance) cov11
	      (slot-value y-schema 'chi^2) chi^2
	      (slot-value y-schema 'independent-var) x-name)))))

(defmethod fit-estimate ((table column-major-table)
			 (y-schema ax-least-squares-column-schema)
			 x-value)
  (let ((x-schema (find-column-schema (independent-var y-schema) table)))
    (gsll:multiplier-estimate
     (funcall (value-normalizer x-schema)
	      x-value x-schema)
     (fit-coeffs y-schema) (covariance y-schema))))
