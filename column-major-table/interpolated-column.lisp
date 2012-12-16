(in-package :numeric-table)

(export '(interpolation-data independent-var init-column-interp interp-column))

(defclass interpolated-column-schema (foreign-column-schema)
  ((interpolation-data
    :accessor interpolation-data
    :initform nil
    :documentation "Data for column-interpolation

In a column-major-table, one column holds the independent variable,
while others are dependent variables.  Each of the dependent columns
holds its interpolation data")
   (independent-var
    :accessor independent-var
    :initform nil
    :documentation "Name of column with independent data")
   (acceleration
    :accessor acceleration
    :initform (gsll:make-acceleration)
    :documentation "GSL's internal interpolation data that accelerates
the interpolation on subsequent calls"))
  (:documentation "Stores data in foreign vectors format for interpolation via GSLL.

The schema stores GSL's interpolation object, the name of the
independent variable and the GSL accelerator"))

(defmethod make-column-schema  (name (type (eql 'interpolated-column-schema))
			&key documentation &allow-other-keys)
    (make-instance 'interpolated-column-schema
		   :name name
		   :comparator #'<
		   :equality-predicate #'=
		   :default-type 'number
		   :documentation documentation))

(defmethod init-column-interp (y-name
			       x-name
			       (column-table numeric-table))
  (let ((y-schema (find-column-schema y-name column-table))
	(x-schema (find-column-schema x-name column-table))
	(table-schema (table-schema column-table)))
    (assert (typep y-schema 'interpolated-column-schema) ()
	    "Column-name: ~a, must refer to a interpolated-column-schema"
	    y-name)
    (assert (typep x-schema 'foreign-column-schema) ()
	    "Column-name: ~a, must refer to a foreign-column-schema"
	    x-name)
    (setf (interpolation-data y-schema)
	  (gsll:make-spline gsll:+cubic-spline-interpolation+
			    (aref (table column-table)
				  (position x-name table-schema :key #'name))
			    (aref (table column-table)
				  (position y-name table-schema :key #'name)))
	  (independent-var y-schema) x-name)))
  

(defmethod interp-column (name value (column-table numeric-table))
  (gsll:evaluate
   (interpolation-data
    (find-column-schema name column-table)) value
    :acceleration (acceleration (find-column-schema name column-table))))

(define-test init-column-interp
  "Test column interpolation on x vs x^2 where x is a vector of
integer values from 0 to 9

I had to set lisp-unit:*epsilon* to 1e-4 in order for test to succeed.
I thought that spline interpolation would correctly interpolate a
polynomical."
  (let ((values
	 (loop for i below 10
	    collect (float i 1d0) into x
	    collect (float (expt i 2) 1d0) into y
	    finally (return (list x y)))))
    (let ((x (grid:make-foreign-array 'double-float :dimensions 10
				      :initial-contents (first values)))
	  (y (grid:make-foreign-array 'double-float :dimensions 10
				      :initial-contents (second values))))
      (let ((table (make-table 'column-major-table
			       (make-table-schema 'column-major-table
						  '((x-col foreign-double-float)
						    (y-col interpolated-column-schema))))))
	(set-table-column table 0 x)
	(set-table-column table 1 y)
	(init-column-interp 'y-col 'x-col table)
	(let ((*epsilon* 1e-4))
	  (assert-number-equal (expt 5.2 2)
			       (interp-column 'y-col 5.2 table)))))))

    
