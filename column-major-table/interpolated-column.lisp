(in-package :numeric-table)

(export '(interpolation-data independent-var init-column-interp interp-column))

(defclass interpolated-column-schema (foreign-double-schema)
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

(add-column-schema-short+long-names 'interpolated-column
				    'interpolated-column-schema)


(defgeneric init-column-interp (table y-column)
  (:documentation "Initialize column interpolation for TABLE's Y-COLUMN

TABLE is a column major table
Y-COLUMN is either a column name or a column schema
X-VALUE is a number")
  (:method ((table column-major-table) (column-name symbol))
    (init-column-interp table (find-column-schema column-name table))))

(defmethod init-column-interp ((column-table numeric-table)
			       (y-schema interpolated-column-schema))
  (let* ((x-schema (find-column-schema
		   (independent-var y-schema) column-table))
	 (x-name (column-name x-schema))
	 (y-name (column-name y-schema))
	 (table-schema (table-schema column-table)))
    (assert (typep x-schema 'foreign-column-schema) ()
	    "Column-name: ~a, must refer to a foreign-column-schema"
	    x-name)
    (setf (interpolation-data y-schema)
	  (gsll:make-spline gsll:+cubic-spline-interpolation+
			    (aref (table-data column-table)
				  (position x-name table-schema :key #'column-name))
			    (aref (table-data column-table)
				  (position y-name table-schema :key #'column-name)))
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
						  '((x-col foreign-double)
						    (y-col interpolated-column))))))
	(set-nth-column 0 table x)
	(set-nth-column 1 table y)
	(setf (independent-var (find-column-schema 'y-col table)) 'x-col)
	(init-column-interp table 'y-col)
	(let ((*epsilon* 1e-4))
	  (assert-number-equal (expt 5.2 2)
			       (interp-column 'y-col 5.2 table)))))))

    
