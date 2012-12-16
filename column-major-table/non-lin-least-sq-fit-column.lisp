(in-package :numeric-table)

(export '(n-coeffs fit-fun fit-fun-jacobian fit-method independent-var sigma
	  init-column-fit fit-column))


(defclass non-lin-ls-sq-column-schema (foreign-column-schema)
  ((n-coeffs
    :accessor n-coeffs
    :initform nil
    :documentation "Number of fitting coefficients.

Set via INIT-COLUMN-FIT")
   (fit-fun
    :accessor fit-fun
    :initform nil
    :documentation "Fitting function of arguments X, fit-coeffs.

Set via INIT-COLUMN-FIT
")
   (fit-fun-jacobian
    :accessor fit-fun-jacobian
    :initform nil
    :documentation "The Jacobian of the fitting function with respect
    to its coefficients.  It's arguments are X and fit-coeffs

Set via INIT-COLUMN-FIT")
   (fit-method
    :accessor fit-method
    :initform gsll:+levenberg-marquardt+
    :documentation "fitting method, such as +levenberg-marquardt+

Set via INIT-COLUMN-FIT")
   (independent-var
    :accessor independent-var
    :initform nil
    :documentation "Name of column with independent data")
   (sigma
    :accessor sigma
    :initform nil
    :documentation "Name of column or a double-float specifying the sigma
Set via INIT-COLUMN-FIT")
   ;;; The following are private slots
   (residual+jacobian-fun
    :initform nil
    :documentation "Function calculates residual and jacobian values.

The function arguments are COEFFS, RESIDUALS, JACOBIAN.  The latter
two are destructively modified with new values.

This is a private function that is generated on the fly when the
fitting is initialized in INIT-COLUMN-FIT.")
   (load-residuals-fun
    :initform nil
    :documentation "Function that calculates the residuals

The function arguments are COEFFS and RESIDUALS.  The latter is
destructively modified with new values.

This is a private function that is generated on the fly when the
fitting is initialized in INIT-COLUMN-FIT.")
   (load-jacobian-fun
    :initform nil
    :documentation "Function that calculates Jacobian matrix of
fitting function.

The function arguments are COEFFS and JACOBIAN.  The latter is
destructively modified with new values.

This is a private function that is generated on the fly when the
fitting is initialized in INIT-COLUMN-FIT."))
  (:documentation "Stores data (both public and private) for nonlinear
  least square fitting via GSLL.

The schema stores GSLL's method, and the functions that will be called by ..."))

(defmethod make-column-schema  (name (type (eql 'non-lin-ls-sq-column-schema))
			&key documentation &allow-other-keys)
  "Makes instance of a non-lin-ls-sq-column-schema.

It replicates the code for foreign-column-schema.  Can I remove this
duplication?"
    (make-instance 'non-lin-ls-sq-column-schema
		   :name name
		   :comparator #'<
		   :equality-predicate #'=
		   :default-type 'number
		   :documentation documentation))

(defmethod init-column-fit (y-name
			    x-name
			    sigma-name-or-val
			    (column-table column-major-table)
			    fit-fun
			    fit-fun-jacobian
			    n-coeffs)
  "Initializes a column before we call non-lin-fit

Syntax:

init-column-fit y-name x-name table fit-fun fit-fun-jacobian n-coeffs

Arguments and Values:

- y-name - Dependent variable column name, a symbol
- x-name - Independent variable column name, a symbol
- sigma-name-or-val - uncertainty in y.  A column name (a symbol), or a double float
- table - a numeric-table
- fit-fun - The fitted function, a function designator
- fit-fun-jacobian - The jacobian of the fit function, a function designator
- n-coeffs - Number of coefficients of fit-fun, an integer

Description:

INIT-COLUMN-FIT performs initializations necessary to call GSLL's non-linear fitting functions on the data of the dependent variable.  It performs the following:
- Initializes and stores values in the column schema
- builds & compiles functions that GSLL will use to run the non-linear
fitting function.  Stores the functions in the column schema.

Consult the non-lin-ls-sq-column-schema class documentation to see
which slots are populated by INIT-COLUMN-FIT.

Example:

 (init-column-fit 'y-var 'x-var some-table #'some-fun #'some-other-fun 3)

"
  (let ((y-schema (find-column-schema y-name column-table))
	(x-schema (find-column-schema x-name column-table))
	sigma-schema)
    (assert (typep y-schema 'non-lin-ls-sq-column-schema) ()
	    "Column-name: ~a, must refer to a non-lin-ls-sq-column-schema"
	    y-name)
    (assert (typep x-schema 'foreign-column-schema) ()
	    "Column-name: ~a, must refer to a foreign-column-schema"
	    x-name)
    (when (symbolp sigma-name-or-val)
      (setf sigma-schema (find-column-schema sigma-name-or-val column-table))
      (assert (typep sigma-schema 'foreign-column-schema) ()
	      "Column-name: ~a, must refer to a foreign-column-schema"
	      sigma-name-or-val))
    (setf (fit-fun y-schema) fit-fun
	  (fit-fun-jacobian y-schema) fit-fun-jacobian
	  (independent-var y-schema) x-name
	  (n-coeffs y-schema) n-coeffs)
    (let ((y (table-column y-name column-table))
	  (x (table-column x-name column-table))
	  (sigma (if (symbolp sigma-name-or-val)
		     (table-column sigma-name-or-val column-table)
		     sigma-name-or-val))
	  ;; the following two will be passed to GSLL
	  (load-residuals-fun (gensym "LOAD-RESIDUALS-FUN"))
	  (load-jacobian-fun (gensym "LOAD-JACOBIAN-FUN")))
      (setf (symbol-function load-residuals-fun)
	    (if (symbolp sigma)
		(lambda (coeffs residuals)
		  "
Syntax:

/lambda/ coeffs residuals => (values)

Arguments and Values:

COEFFS - vector of function coefficients
RESIDUALS - vector of length N of residuals

Description:

Calls function FIT-FUN for each value of X, computes residual, which
is stored in the RESIDUALS vector (a grid).

Destructively modifies RESIDUALS with new values.

This is the first of the three functions that are arguments to gsll's
make-nonlinear-fdffit.

This function is created on-the-fly by INIT-COLUMN-FIT using the
provided fit-functions and other data.

FIT-FUN, X, and Y are provided via a closure.
"
		  (dotimes (i (row-count column-table))
		    (setf (grid:aref residuals i)
			  (/ (- (funcall fit-fun (grid:aref x i) coeffs)
				(grid:aref y i))
			     (grid:aref sigma i))))
		  (values))
		(lambda (coeffs residuals)
		  "
Syntax:

/lambda/ coeffs residuals => (values)

Arguments and Values:

COEFFS - vector of function coefficients
RESIDUALS - vector of length N of residuals

Description:

Calls function FIT-FUN for each value of X, computes residual, which
is stored in the RESIDUALS vector (a grid).

Destructively modifies RESIDUALS with new values.

This is the first of the three functions that are arguments to gsll's
make-nonlinear-fdffit.

This function is created on-the-fly by INIT-COLUMN-FIT using the
provided fit-functions and other data.

FIT-FUN, X, and Y are provided via a closure.
"
		  (dotimes (i (row-count column-table))
		    (setf (grid:aref residuals i)
			  (/ (- (funcall fit-fun (grid:aref x i) coeffs)
				(grid:aref y i))
			     sigma)))
		  (values)))
	    (slot-value y-schema 'load-residuals-fun)
	    load-residuals-fun)
      (setf (symbol-function load-jacobian-fun)
	    (lambda (coeffs jacobian)
	      "
Syntax:

/lambda/ coeffs jacobian => (values)

Arguments and Values:

COEFFS - vector of function coefficients (a grid)
JACOBIAN - matrix of dimension MxN

Description:

Calls function FIT-FUN for each value of X, computes Jacobian for X,
which is stored in the JACOBIAN.  The JACOBIAN columns store values
for a fixed X and rows store values of the derivative for one
coefficient.

Destructively modifies JACOBIAN with new values.

This is the second of the three functions that are arguments to gsll's
make-nonlinear-fdffit.

This function is created on-the-fly by INIT-COLUMN-FIT using the
provided fit-fun-jacobian and other data.

FIT-FUN-JACOBIAN, X and a few other variables are provided via a
closure.
"
	      (dotimes (i (row-count column-table))
		(let ((jacobian-column (funcall fit-fun-jacobian (grid:aref x i)
						coeffs)))
		  (loop :for j :below n-coeffs
		     :for jacobian-element :in jacobian-column
		     :do (setf (grid:aref jacobian i j)
				      jacobian-element))))
	      (values))
	    (slot-value y-schema 'load-jacobian-fun)
	    load-jacobian-fun))
    (let ((fdf-fun (gensym "FDF"))
	  (f-residual (slot-value y-schema 'load-residuals-fun))
	  (f-residual-derivative (slot-value y-schema 'load-jacobian-fun)))
      (setf (symbol-function fdf-fun)
	    (lambda (coeffs residuals jacobian)
	      "
Syntax:

/lambda/ coeffs residuals jacobian

Arguments and Values:

COEFFS - vector of function coefficients
RESIDUAL - vector of length N of residuals
JACOBIAN - matrix of dimension MxN

Calls other functions to calculate RESIDUALS and JACOBIAN for each X.  Destructively modifies RESIDUALS and JACOBIAN with new values.

This is the third of the three functions that are arguments to gsll's
make-nonlinear-fdffit.  It calls the first two of the set.

This function is created on-the-fly by INIT-COLUMN-FIT using the
provided fit-functions and other data.
"
	      (funcall f-residual coeffs residuals)
	      (funcall f-residual-derivative coeffs jacobian)
	      (values))
	    (slot-value y-schema 'residual+jacobian-fun) fdf-fun))))

(defparameter *b* 1.5d0)
(defparameter *a* 1.1d0)

(defun y (x coeffs)
  (let ((a (grid:aref coeffs 0))
	(b (grid:aref coeffs 1)))
    (* a (exp (- (* b x))))))

(defun dy/d-coeffs (x coeffs)
  (let* ((a (grid:aref coeffs 0))
	 (b (grid:aref coeffs 1))
	 (e (exp (- (* b x)))))
    (list e
	  (- (* a x e)))))

(defparameter *x_i*
  (grid:make-grid '((grid:foreign-array 4) double-float)
		    :initial-contents
		    (loop for i below 4
		       collect (float i 1d0))))
(defparameter *y_i*
  (let ((coeffs (make-array 2 :initial-contents (list *a* *b*))))
    (grid:make-grid '((grid:foreign-array 4) double-float)
		    :initial-contents
		    (loop for i below 4
		       collect (float (y i
					 coeffs) 1d0))))
  "Fit data corresponsing to coefficients *a* and *b*")




(define-test init-column-fit
  "This test exercises INIT-COLUMN-FIT, and the functions that it creates."
  (let ((table (make-table 'column-major-table
			   (make-table-schema 'column-major-table
					      '((x-col foreign-double-float)
						(y-col non-lin-ls-sq-column-schema))))))
    (set-table-column table 0 *x_i*)
    (set-table-column table 1 *y_i*)
    (init-column-fit 'y-col 'x-col 1d0 table
		     #'y #'dy/d-coeffs 2)
    (let* ((schema (find-column-schema 'y-col table))
	   (coeffs (make-array 2 :initial-contents (list (+ *a* *a*) *b*)))
	   (residuals (grid:make-grid '((grid:foreign-array 4) double-float)))
	   (residuals-fun (slot-value schema 'load-residuals-fun)))
      (funcall residuals-fun coeffs residuals)
      (assert-numerical-equal *y_i* residuals "Residual=y when we double amplitude"))
    (let* ((schema (find-column-schema 'y-col table))
	   (coeffs (make-array 2 :initial-contents (list *a* *b*)))
	   (residuals (grid:make-grid '((grid:foreign-array 4) double-float)))
	   (residuals-fun (slot-value schema 'load-residuals-fun)))
      (funcall residuals-fun coeffs residuals)
      (assert-numerical-equal (grid:make-grid '((grid:foreign-array 4) double-float)
					      :initial-element 0d0)
			      residuals "Residuals=0 when coeffs are exact"))
    (let* ((schema (find-column-schema 'y-col table))
	   (coeffs (make-array 2 :initial-contents (list *a* *b*)))
	   (jacobian (grid:make-grid '((grid:foreign-array 4 2) double-float)))
	   (jacobian-fun (slot-value schema 'load-jacobian-fun)))
      (funcall jacobian-fun coeffs jacobian)
      (assert-numerical-equal
       (grid:make-grid '((grid:foreign-array 4) double-float)
		       :initial-contents
		       (loop for x below 4
			  collect (- (exp (- (* *b* x))))))
       (grid:column jacobian 0) "Jacobian column 0")
      (assert-numerical-equal
       (grid:make-grid '((grid:foreign-array 4) double-float)
		       :initial-contents
		       (loop for x below 4
			  collect (* *a* x (exp (- (* *b* x))))))
       (grid:column jacobian 1) "Jacobian column 1"))))


(defun norm-f (fit)
  "Find the norm of the fit function f."
  (gsll:euclidean-norm (gsll:function-value fit)))

(defmethod fit-column ((table column-major-table) y-name coeffs-guess
		       &optional print-steps (max-steps 25))
  "Fits column data to function specified in column schema

Syntax:

fit-column table y-name coeffs-guess

Arguments and Values:

table - a column-major-table
y-name - name of column, a symbol
coeffs-guess - Guesses of fit function coefficients, a list
print-steps - A boolean
max-steps - Maximum number of iterations, an integer

Description:

FIT-COLUMN iteratively calls GSLL's ITERATE on GSLL's NONLINEAR-FDFFIT
object.  The parameters of NONLINEAR-FDFFIT and the fitting method are
obtained from the column schema.

This function is based on GSLL's NONLINEAR-LEAST-SQUARES-EXAMPLE"
  (let* ((column-schema (find-column-schema y-name table))
	 (n-coeffs (n-coeffs column-schema))
	 (n-data (row-count table))
	 (method (fit-method column-schema))
	 (residuals-fun (slot-value column-schema 'load-residuals-fun))
	 (jacobian-fun (slot-value column-schema 'load-jacobian-fun))
	 (residuals+jacobian-fun (slot-value column-schema 'residual+jacobian-fun))
	 covariance
	 (fit (gsll:make-nonlinear-fdffit
	       method
	       (list n-data n-coeffs)
	       (list residuals-fun jacobian-fun residuals+jacobian-fun)
	       coeffs-guess nil)))
    (macrolet ((fitx (i) `(grid:aref (gsll:solution fit) ,i))
	       (err (i) `(sqrt (grid:aref covariance ,i ,i))))
      (when print-steps
	(format t "iter: ~d x = ~{~15,8f ~} |f(x)|=~7,6g~&"
		0 (loop for i below n-coeffs
		       collect (fitx i))
		(norm-f fit)))
      (loop for iter from 0 below max-steps
	 until
	   (and (plusp iter)
		(gsll:fit-test-delta fit 1.0d-4 1.0d-4))
	 do
	   (gsll:iterate fit)
	   (setf covariance (gsll:ls-covariance fit 0.0d0 covariance))
	   (when print-steps
	     (format t "iter: ~d x = ~{~15,8f ~} |f(x)|=~7,6g~&"
		     (1+ iter) (loop for i below n-coeffs
				  collect (fitx i))
		     (norm-f fit)))
	 finally
	   (let* ((chi (norm-f fit))
		  (dof (- n-data n-coeffs))
		  (c (max 1.0d0 (/ chi (sqrt dof)))))
	     (when print-steps
	       (format t "chisq/dof = ~g~&" (/ (expt chi 2) dof))
	       (dotimes (i n-coeffs)
		 (format t "c_~g         = ~,5f +/- ~,5f~&" i (fitx i) (* c (err i)))))
	     (return (loop for i below n-coeffs
			collect (fitx i))))))))


(define-test init-column-fit
  ""
  (let ((table (make-table 'column-major-table
			   (make-table-schema 'column-major-table
					      '((x-col foreign-double-float)
						(y-col non-lin-ls-sq-column-schema))))))
    (set-table-column table 0 *x_i*)
    (set-table-column table 1 *y_i*)
    (init-column-fit 'y-col 'x-col 1d0 table
		     #'y #'dy/d-coeffs 2)
    (let ((lisp-unit:*epsilon* 1e-9))
      (assert-numerical-equal '(1.1 1.5)
			    (fit-column table 'y-col
					(grid:make-grid '((grid:foreign-array 2) double-float)
							:initial-contents (list 2d0 1d0)))))))


(defun exp-y (x coeffs)
  (let ((a (grid:aref coeffs 0))
	(lambda (grid:aref coeffs 1))
	(b (grid:aref coeffs 2)))
    (+ (* A (exp (* (- lambda) x))) b)))

(defun exp-dy/d-coeffs (x coeffs)
  (let* ((a (grid:aref coeffs 0))
	 (lambda (grid:aref coeffs 1))
	 (e (exp (- (* lambda x)))))
    (list e
	  (* -1 x a e)
	  1d0)))


(define-test gsl-fit
  ""
  (let ((table (make-table 'column-major-table
			   (make-table-schema 'column-major-table
					      '((x-col foreign-double-float)
						(y-col non-lin-ls-sq-column-schema)))))
	(data (gsll::generate-nlls-data)))
    (set-table-column table 0 
		      (grid:make-grid '((grid:foreign-array 40) double-float)
				      :initial-contents
				      (loop for i below 40
					   collect i)))
    (set-table-column table 1 (gsll::exponent-fit-data-y data))
    (init-column-fit 'y-col 'x-col table
		     #'exp-y #'exp-dy/d-coeffs 3)
    (let ((lisp-unit:*epsilon* 1e-1))
      (assert-numerical-equal '(5 0.1 1)
			    (fit-column table 'y-col
					(grid:make-grid '((grid:foreign-array 3) double-float)
							:initial-contents (list 2 1 1)))))))

(defun |sigma| (c n)
  (/ (- 1 (expt c (+ 1 n)))
     (- 1 c)))

(defun |sigma-prim| (c n)
  (/ (+ (* (expt c n)
	   (- (* n (- c 1))
	      1))
	1)
     (- 1 (expt c 2))))
