(in-package :numeric-table)

;;; The class design can probably be tweaked.  For some reason, the
;;; `interpolation-method' accessor is not available, and I have to
;;; use slot-value.


(export '(interpolation-data init-column-interp interp-column))

(defclass interpolated-column-schema (column-fit+interpolation
				      foreign-double-schema)
  ((method :accessor interpolation-method)
   (interpolation-data
    :accessor interpolation-data
    :initform nil
    :documentation "Data for column-interpolation

In a column-major-table, one column holds the independent variable,
while others are dependent variables.  Each of the dependent columns
holds its interpolation data")
   (acceleration
    :accessor acceleration
    :documentation "GSL's internal interpolation data that accelerates
the interpolation on subsequent calls"))
  (:documentation "Stores data in foreign vectors format for interpolation via GSLL.

The schema stores GSL's interpolation object, the name of the
independent variable and the GSL accelerator"))

(add-column-schema-short+long-names 'interpolated-column
				    'interpolated-column-schema)


(defmethod describe-object :after ((self interpolated-column-schema) stream)
  (format stream "Interpolation method: ~a~%" (slot-value self 'method)))

(defgeneric init-column-interp (table y-column method)
  (:documentation "Initialize column interpolation for TABLE's Y-COLUMN

TABLE is a column major table
Y-COLUMN is either a column name or a column schema
X-VALUE is a number")
  (:method ((table column-major-table) (column-name symbol) method)
    (init-column-interp table (find-column-schema column-name table)
			method)))


(defmethod init-column-interp :around ((column-table numeric-table)
				       (y-schema interpolated-column-schema)
				       method)
  (let* ((x-schema (find-column-schema
		    (col-independent-var y-schema) column-table))
	 (x-name (column-name x-schema)))
    (setf (interpolation-method y-schema) method
	  (col-independent-var y-schema) x-name
	  (acceleration y-schema) (gsll:make-acceleration))
    (call-next-method)))

(defmethod init-column-interp ((column-table numeric-table)
			       (y-schema interpolated-column-schema)
			       (method (eql 'cubic-spline-interpolation)))
  (let* ((x-schema (find-column-schema
		    (col-independent-var y-schema) column-table))
	 (x-name (column-name x-schema))
	 (y-name (column-name y-schema))
	 (table-schema (table-schema column-table)))
    (setf (interpolation-data y-schema)
	  (gsll:make-spline gsll:+cubic-spline-interpolation+
			    (aref (table-data column-table)
				  (position x-name table-schema :key #'column-name))
			    (aref (table-data column-table)
				  (position y-name table-schema :key #'column-name))))))

(defmethod init-column-interp ((column-table numeric-table)
			       (y-schema interpolated-column-schema)
			       (method (eql 'linear-interpolation)))
  (let* ((x-schema (find-column-schema
		    (col-independent-var y-schema) column-table))
	 (x-name (column-name x-schema))
	 (y-name (column-name y-schema))
	 (table-schema (table-schema column-table)))
    (setf (interpolation-data y-schema)
	  (gsll:make-interpolation
	   gsll:+linear-interpolation+
	   (aref (table-data column-table)
		 (position x-name table-schema :key #'column-name))
	   (aref (table-data column-table)
		 (position y-name table-schema :key #'column-name))))))

(defmethod init-column-interp ((column-table numeric-table)
			       (y-schema interpolated-column-schema)
			       (method (eql 'polynomial-interpolation)))
  (let* ((x-schema (find-column-schema
		    (col-independent-var y-schema) column-table))
	 (x-name (column-name x-schema))
	 (y-name (column-name y-schema))
	 (table-schema (table-schema column-table)))
    (setf (interpolation-data y-schema)
	  (gsll:make-interpolation
	   gsll:+polynomial-interpolation+
	   (aref (table-data column-table)
		 (position x-name table-schema :key #'column-name))
	   (aref (table-data column-table)
		 (position y-name table-schema :key #'column-name))))))
  

(defmethod interp-column (name value (column-table numeric-table))
  (let* ((y-column-schema (find-column-schema name column-table))
	 (y-index (i-column y-column-schema))
	 (y-data (aref (table-data column-table) y-index))
	 (x-name (col-independent-var y-column-schema))
	 (x-column-schema (find-column-schema x-name column-table))
	 (x-index (i-column x-column-schema))
	 (x-data (aref (table-data column-table) x-index)))
    ;; The syntax to gsll::evaluate depends on whether we use a spline
    ;; or other interpolation methods.  This case statement could be
    ;; circumvented by creating a class structure for the methods.
    ;; However, this class structure will be tied to gsll.
    (case (print (slot-value y-column-schema 'method))
      (cubic-spline-interpolation
       (gsll:evaluate (interpolation-data y-column-schema)
		      value
		      :acceleration
		      (acceleration y-column-schema)))
      (t
       (gsll:evaluate (interpolation-data y-column-schema)
		      value
		      :xa x-data
		      :ya y-data
		      :acceleration
		      (acceleration y-column-schema))))))

(define-test spline-column-interp
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
	(setf (nth-column 0 table) x
	      (nth-column 1 table) y)
	(setf (col-independent-var (find-column-schema 'y-col table)) 'x-col)
	(init-column-interp table 'y-col 'cubic-spline-interpolation)
	(let ((*epsilon* 1e-4))
	  (assert-number-equal (expt 5.2 2)
			       (interp-column 'y-col 5.2 table)))))))

(define-test linear-column-interp
  "Test column interpolation on x vs x^2 where x is a vector of
integer values from 0 to 9

I had to set lisp-unit:*epsilon* to 1e-4 in order for test to succeed.
I thought that spline interpolation would correctly interpolate a
polynomical."
  (let ((x (grid:make-foreign-array 'double-float :dimensions 3
				    :initial-contents '(1d0 2d0 3d0)))
	(y (grid:make-foreign-array 'double-float :dimensions 3
				    :initial-contents '(1d0 2d0 4d0))))
    (let ((table (make-table 'column-major-table
			     (make-table-schema 'column-major-table
						'((x-col foreign-double)
						  (y-col interpolated-column))))))
      (setf (nth-column 0 table) x
	    (nth-column 1 table) y)
      (setf (col-independent-var (find-column-schema 'y-col table)) 'x-col)
      (init-column-interp table 'y-col 'linear-interpolation)
      (describe-object (find-column-schema 'y-col table) t)
      (let ((*epsilon* 1e-4))
	(assert-number-equal 1.5d0
			     (interp-column 'y-col 1.5d0 table))
	(assert-number-equal 3d0
			     (interp-column 'y-col 2.5d0 table))))))

(define-test polynomial-column-interp
  "Test column interpolation on x vs x^2 where x is a vector of
integer values from 0 to 9

I had to set lisp-unit:*epsilon* to 1e-4 in order for test to succeed.
I thought that spline interpolation would correctly interpolate a
polynomical."
  (let ((x (grid:make-foreign-array 'double-float :dimensions 3
				    :initial-contents '(1d0 2d0 3d0)))
	(y (grid:make-foreign-array 'double-float :dimensions 3
				    :initial-contents '(2d0 5d0 10d0))))
    (let ((table (make-table 'column-major-table
			     (make-table-schema 'column-major-table
						'((x-col foreign-double)
						  (y-col interpolated-column))))))
      (setf (nth-column 0 table) x
	    (nth-column 1 table) y)
      (setf (col-independent-var (find-column-schema 'y-col table)) 'x-col)
      (init-column-interp table 'y-col 'polynomial-interpolation)
      (let ((*epsilon* 1e-4))
	(assert-number-equal 3.25d0
			     (interp-column 'y-col 1.5d0 table))
	(assert-number-equal 7.25d0
			     (interp-column 'y-col 2.5d0 table))))))

    


#|

(let ((x (grid:make-foreign-array 'double-float :dimensions 3
				  :initial-contents '(1d0 2d0 3d0)))
      (y (grid:make-foreign-array 'double-float :dimensions 3
				  :initial-contents '(1d0 2d0 4d0))))
  (let ((interpolation (gsll:make-interpolation gsll:+linear-interpolation+
						y x)))
    (list
     (gsll:evaluate interpolation 1.5d0 :xa x :ya y)
     (gsll:evaluate interpolation 2.5d0 :xa x :ya y))))

(let ((x-arr (grid:make-foreign-array 'double-float :dimensions 3
				  :initial-contents '(1d0 2d0 3d0)))
      (y-arr (grid:make-foreign-array 'double-float :dimensions 3
				  :initial-contents '(1d0 2d0 4d0)))
      (x 2.1d0))
  (list
   (let ((interpolation (gsll:make-interpolation gsll:+cubic-spline-interpolation+
						 y-arr x-arr)))
     (gsll:evaluate interpolation x :xa x-arr :ya y-arr))
   (let ((interpolation (gsll:make-spline gsll:+cubic-spline-interpolation+
					  y-arr x-arr)))
     (gsll:evaluate interpolation x))))


|#
