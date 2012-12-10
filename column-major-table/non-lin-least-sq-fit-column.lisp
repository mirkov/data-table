(in-package :numeric-table)

(defclass non-lin-ls-sq-column-schema (foreign-column-schema)
  ((n-coeffs
    :accessor n-coeffs
    :initform nil
    :documentation "Number of fitting coefficients")
   (fit-fun
    :accessor fit-fun
    :initform nil
    :documentation "Fitting function of arguments X, fit-coeffs
")
   (fit-fun-jacobian
    :accessor fit-fun-jacobian
    :initform nil
    :documentation "The Jacobian of the fitting function with respect
    to its coefficients.  Function of X, fit-coeffs")
   (fit-method
    :accessor fit-method
    :initform gsll:+levenberg-marquardt+
    :documentation "fitting method, such as +levenberg-marquardt+")
   #+skip(residual-fun
    :reader residual-fun
    :initform :residual-fun
    :documentation "Residual function.  

This is a function of a single argument X.  The fitting parameters are
passed by special variables.  It returns a single value.")
   #+skip(jacobian-fun
    :reader jacobian-fun
    :initform :jacobian-fun
    :documentation "Residual derivative function, can be NIL

This is a function of a single argument X.  The fitting parameters are
passed by special variables.  It returns a vector of residual
derivatives with respect to each fitting parameter.")
   (independent-var
    :accessor independent-var
    :initform nil
    :documentation "Name of column with independent data")
   ;;; The following are private slots
   (residual+jacobian-fun
    :initform nil
    :documentation "Function returning residual and jacobian values.
    This is a private function that is generated on the fly when the
    fitting is initialized.")
   (load-residuals-fun
    :initform nil
    :documentation "Internal function that loads the residuals vector.
This is a symbol, whose function value is set when the fitting is
initialized")
   (load-jacobian-fun
    :initform nil
    :documentation "Internal function that loads Jacobian matrix of
the residuals.  This is a symbol, whose function value is set when the
fitting is initialized"))
  (:documentation "Stores data (both public and private) for nonlinear
  least square fitting via GSLL.

The schema stores GSLL's method, and the functions that will be called by ..."))

(defmethod make-column-schema  (name (type (eql 'non-lin-ls-sq-column-schema))
			&key documentation &allow-other-keys)
    (make-instance 'non-lin-ls-sq-column-schema
		   :name name
		   :comparator #'<
		   :equality-predicate #'=
		   :default-type 'number
		   :documentation documentation))

(defmethod init-column-fit (y-name
			    x-name
			    (column-table numeric-table)
			    fit-fun
			    fit-fun-jacobian
			    n-coeffs)
  (let ((y-schema (find-column-schema y-name column-table))
	(x-schema (find-column-schema x-name column-table))
	#+skip(table-schema (table-schema column-table)))
    (assert (typep y-schema 'non-lin-ls-sq-column-schema) ()
	    "Column-name: ~a, must refer to a non-lin-ls-sq-column-schema"
	    y-name)
    (assert (typep x-schema 'foreign-column-schema) ()
	    "Column-name: ~a, must refer to a foreign-column-schema"
	    x-name)
    (setf (fit-fun y-schema) fit-fun
	  (fit-fun-jacobian y-schema) fit-fun-jacobian
	  (independent-var y-schema) x-name
	  (n-coeffs y-schema) n-coeffs)
    (let ((y (table-column y-name column-table))
	  (x (table-column x-name column-table))
	  (load-residuals-fun (gensym "LOAD-RESIDUALS-FUN"))
	  (load-jacobian-fun (gensym "LOAD-JACOBIAN-FUN")))
      (setf (symbol-function load-residuals-fun)
	    (lambda (coeffs residuals)
	      "Syntax:
/lambda/ coeffs residual => (values)

Arguments:
COEFFS - vector of function coefficients
RESIDUAL - vector of length N of residuals

Description: Calls function FIT-FUN for each value of X, computes
residual, which is stored in RESIDUAL.

FIT-FUN, X, and Y are provided via a closure.
"
	      (dotimes (i (row-count column-table))
		(setf (grid:aref residuals i)
		      (- (funcall fit-fun (grid:aref x i) coeffs)
			 (grid:aref y i))))
	      (values))
	    (slot-value y-schema 'load-residuals-fun)
	    load-residuals-fun)
      (setf (symbol-function load-jacobian-fun)
	    (lambda (coeffs jacobian)
	      (dotimes (i (row-count column-table))
		(let ((jacobian-column (funcall fit-fun-jacobian (grid:aref x i)
						coeffs)))
		  (loop :for j :below n-coeffs
		     :for jacobian-element :in jacobian-column
		     :do (setf (grid:aref jacobian i j)
				      (- jacobian-element)))))
	      (values))
	    (slot-value y-schema 'load-jacobian-fun)
	    load-jacobian-fun))
    (let ((fdf-fun (gensym "FDF"))
	  (f-residual (slot-value y-schema 'load-residuals-fun))
	  (f-residual-derivative (slot-value y-schema 'load-jacobian-fun)))
      (setf (symbol-function fdf-fun)
	    (lambda (coeffs residuals jacobian)
	      "Given the vector X and residual vector F and derivative
matrix JACOBIAN call the residual function and residual derivative
function to fill F and JACOBIAN"
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
    (list (- e)
	  (* a x e))))

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
  ""
  (let ((table (make-table 'column-major-table
			   (make-table-schema 'column-major-table
					      '((x-col foreign-double-float)
						(y-col non-lin-ls-sq-column-schema))))))
    (set-table-column table 0 *x_i*)
    (set-table-column table 1 *y_i*)
    (init-column-fit 'y-col 'x-col table
		     #'y #'dy/d-coeffs 2)
    (let* ((schema (find-column-schema 'y-col table))
	   (coeffs (make-array 2 :initial-contents (list (+ *a* *a*) *b*)))
	   (residuals (grid:make-grid '((grid:foreign-array 4) double-float)))
	   (residuals-fun (slot-value schema 'load-residuals-fun)))
      (funcall residuals-fun coeffs residuals)
      (assert-numerical-equal *y_i* residuals))
    (let* ((schema (find-column-schema 'y-col table))
	   (coeffs (make-array 2 :initial-contents (list *a* *b*)))
	   (residuals (grid:make-grid '((grid:foreign-array 4) double-float)))
	   (residuals-fun (slot-value schema 'load-residuals-fun)))
      (funcall residuals-fun coeffs residuals)
      (assert-numerical-equal (grid:make-grid '((grid:foreign-array 4) double-float)
					      :initial-element 0d0)
			      residuals))
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
       (grid:column jacobian 0))
      (assert-numerical-equal
       (grid:make-grid '((grid:foreign-array 4) double-float)
		       :initial-contents
		       (loop for x below 4
			  collect (* *a* x (exp (- (* *b* x))))))
       (grid:column jacobian 1)))))


(defun norm-f (fit)
  "Find the norm of the fit function f."
  (gsll:euclidean-norm (gsll:function-value fit)))

(defmethod fit-column ((table column-major-table) y-name coeffs-guess)
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
	       coeffs-guess nil))
	 (print-steps t)
	 (max-steps 25))
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
    (init-column-fit 'y-col 'x-col table
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
    (mapcar #'-
	    (list e
		  (* -1 x a e)
		  1d0))))


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
