(in-package :numeric-table)

;;; The class design can probably be tweaked.  For some reason, the
;;; `interpolation-method' accessor is not available, and I have to
;;; use slot-value.


(export '(init-column-interp
	  linear-interpolation-column polynomial-interpolation-column
	  cubic-spline-interpolation-column periodic-cubic-spline-interpolation-column
	  akima-interpolation-column periodic-akima-interpolation-column))

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

(defclass gsll-interpolation (interpolated-column-schema)
  ((gsll-interpolation-method
    :accessor gsll-interpolation-method
    :documentation "Stores the GSLL's constant spcifying the interpolation method"))
  (:documentation "Base class for interpolation methods that use the GSLL engine"))

(defclass gsll-spline-interpolation (gsll-interpolation)
  ()
  (:documentation "Base class for spline-type interpolation

It is also used for Akima interpolation"))

(defclass gsll-periodic-spline-interpolation (gsll-interpolation)
  ()
  (:documentation "Base class for periodic interpolation

It may be used together with spline interpolations"))

(defclass gsll-other-interpolation (gsll-interpolation)
  ()
  (:documentation "Base class for non-spline-type interpolation"))

(defclass linear-interpolation-column-schema (gsll-other-interpolation)
  ((method :initform 'linear-interpolation)
   (gsll-interpolation-method
    :initform gsll:+linear-interpolation+)))

(defclass polynomial-interpolation-column-schema (gsll-other-interpolation)
  ((method :initform 'polynomial-interpolation)
   (gsll-interpolation-method
    :initform gsll:+polynomial-interpolation+)))

(defclass cubic-spline-interpolation-column-schema (gsll-spline-interpolation)
  ((method :initform 'cubic-spline-interpolation)
   (gsll-interpolation-method
    :initform gsll:+cubic-spline-interpolation+)))

(defclass periodic-cubic-spline-interpolation-column-schema
    (cubic-spline-interpolation gsll-periodic-spline-interpolation)
  ((method :initform 'periodic-cubic-spline-interpolation)
   (gsll-interpolation-method
    :initform gsll:+periodic-cubic-spline-interpolation+)))

(defclass akima-interpolation-column-schema (gsll-spline-interpolation)
  ((method :initform 'akima-interpolation)
   (gsll-interpolation-method
    :initform gsll:+akima-interpolation+)))

(defclass periodic-akima-interpolation-column-schema
    (akima-spline-interpolation gsll-periodic-spline-interpolation)
  ((method :initform 'periodic-akima-interpolation)
   (gsll-interpolation-method
    :initform gsll:+periodic-akima-interpolation+)))



(add-column-schema-short+long-names 'linear-interpolation-column
				    'linear-interpolation-column-schema
				    'polynomial-interpolation-column
				    'polynomial-interpolation-column-schema
				    'cubic-spline-interpolation-column
				    'cubic-spline-interpolation-column-schema
				    'periodic-cubic-spline-interpolation-column
				    'periodic-cubic-spline-interpolation-column-schema
				    'akima-interpolation-column
				    'akima-interpolation-column-schema
				    'periodic-akima-interpolation-column
				    'periodic-akima-interpolation-column-schema)



(defmethod describe-object :after ((self interpolated-column-schema) stream)
  (format stream "Interpolation method: ~a~%" (slot-value self 'method)))

(defgeneric init-column-interp (table y-column)
  (:documentation "Initialize column interpolation for TABLE's Y-COLUMN

TABLE is a column major table
Y-COLUMN is either a column name or a column schema
X-VALUE is a number

The interpolation method is determined based on the column schema
which must be subtype of GSLL-SPLINE-INTERPOLATION or
GSLL-OTHER-INTERPOLATION
")
  (:method ((table column-major-table) (column-name symbol))
    (init-column-interp table (find-column-schema column-name table))))


(defmethod init-column-interp :around ((column-table numeric-table)
				       (y-schema gsll-interpolation))
  (let* ((x-schema (find-column-schema
		    (col-independent-var y-schema) column-table))
	 (x-name (column-name x-schema)))
    (setf (col-independent-var y-schema) x-name
	  (acceleration y-schema) (gsll:make-acceleration))
    (call-next-method)))

(defmethod init-column-interp ((column-table numeric-table)
			       (y-schema gsll-spline-interpolation))
  (let* ((x-schema (find-column-schema
		    (col-independent-var y-schema) column-table))
	 (x-name (column-name x-schema))
	 (y-name (column-name y-schema))
	 (table-schema (table-schema column-table)))
    (setf (interpolation-data y-schema)
	  (gsll:make-spline (gsll-interpolation-method y-schema)
			    (nested-vectors:nth-column
			     (table-data column-table)
			     (position x-name table-schema :key #'column-name))
			    (nested-vectors:nth-column
			     (table-data column-table)
				  (position y-name table-schema :key #'column-name))))))

(defmethod init-column-interp ((column-table numeric-table)
			       (y-schema gsll-other-interpolation))
  (let* ((x-schema (find-column-schema
		    (col-independent-var y-schema) column-table))
	 (x-name (column-name x-schema))
	 (y-name (column-name y-schema))
	 (table-schema (table-schema column-table)))
    (setf (interpolation-data y-schema)
	  (gsll:make-interpolation
	   (gsll-interpolation-method y-schema)
	   (nested-vectors:nth-column (table-data column-table)
		 (position x-name table-schema :key #'column-name))
	   (nested-vectors:nth-column (table-data column-table)
		 (position y-name table-schema :key #'column-name))))))

#+skip(defgeneric interp-column (column value table)
  (:documentation
"Interpolated a table column

The data comes from TABLE.  COLUMN identifies the column that is interpolated.

It is either its name or its schema")
  (:method ((name symbol) value table)
    (let ((column-schema (find-column-schema name table)))
      (interp-column column-schema value table))))

(defmethod evaluate ((column-table numeric-table)
		     (y-column-schema gsll-spline-interpolation) value)
  (gsll:evaluate (interpolation-data y-column-schema)
		 value
		 :acceleration
		 (acceleration y-column-schema)))

(defmethod evaluate ((column-table numeric-table)
			  (y-column-schema gsll-other-interpolation)
			  value)
  (let* ((y-index (i-column y-column-schema))
	 (y-data (nested-vectors:nth-column (table-data column-table) y-index))
	 (x-name (col-independent-var y-column-schema))
	 (x-column-schema (find-column-schema x-name column-table))
	 (x-index (i-column x-column-schema))
	 (x-data (nested-vectors:nth-column (table-data column-table) x-index)))
    (gsll:evaluate (interpolation-data y-column-schema)
		   value
		   :xa x-data
		   :ya y-data
		   :acceleration
		   (acceleration y-column-schema))))

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
						    (y-col cubic-spline-interpolation-column))))))
	(setf (nth-column 0 table) x
	      (nth-column 1 table) y)
	(setf (col-independent-var (find-column-schema 'y-col table)) 'x-col)
	(init-column-interp table 'y-col)
	(let ((*epsilon* 1e-4))
	  (assert-number-equal (expt 5.2 2)
			       (evaluate table 'y-col 5.2)))))))

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
						  (y-col linear-interpolation-column))))))
      (setf (nth-column 0 table) x
	    (nth-column 1 table) y)
      (setf (col-independent-var (find-column-schema 'y-col table)) 'x-col)
      (init-column-interp table 'y-col)
;;      (describe-object (find-column-schema 'y-col table) t)))
      (let ((*epsilon* 1e-4))
	(assert-number-equal 1.5d0
			     (evaluate table 'y-col 1.5d0))
	(assert-number-equal 3d0
			     (evaluate table 'y-col 2.5d0))))))

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
						  (y-col polynomial-interpolation-column))))))
      (setf (nth-column 0 table) x
	    (nth-column 1 table) y)
      (setf (col-independent-var (find-column-schema 'y-col table)) 'x-col)
      (init-column-interp table 'y-col)
      (let ((*epsilon* 1e-4))
	(assert-number-equal 3.25d0
			     (evaluate table 'y-col 1.5d0))
	(assert-number-equal 7.25d0
			     (evaluate table 'y-col 2.5d0))))))

    


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
