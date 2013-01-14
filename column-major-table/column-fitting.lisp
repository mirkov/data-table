(in-package :numeric-table)

(export '(col-independent-var independent-var))

(defclass column-fit (column-fit+interpolation)
  ((sigma
    :accessor sigma
    :initarg sigma
    :initform 1d0
    :documentation "Name of column or a double-float specifying the sigma
Initially set to 1d0")
   (fit-coeffs
    :accessor fit-coeffs
    :initform nil
    :documentation "List of fitting coefficients")
   (chi^2
    :reader chi^2
    :initform nil
    :documentation "chi^2 of the fit"))
  (:documentation "Base class for fitting"))

(defclass linear-column-fit (column-fit)
  ((covariance
    :reader covariance
    :initform nil
    :documentation "Three element list storing the covariance matrix
    elements 00 01 and 11")))


(defclass nonlinear-column-fit (column-fit)
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

Set via INIT-COLUMN-FIT"))
  (:documentation
   "Base class for nonlinear fitting.  This is a virtual class that is
   a base for other fitting classes."))

(defgeneric fit-column (table y-colum &key &allow-other-keys)
  (:documentation "Fit y-column of table

TABLE is a column-major table
Y-COLUMN is a column name or a column schema

Methods can supply additional keyword arguments
")
  (:method ((table column-major-table) (column-name symbol)
	    &rest keyword-arguments
	    &key &allow-other-keys)
    (apply #'fit-column table (find-column-schema column-name table)
	   keyword-arguments)))

(defgeneric fit-estimate (table y-column x-value)
  (:documentation "Calulate the fit estimate for TABLE's Y-COLUMN

TABLE is a column major table
Y-COLUMN is either a column name or a column schema
X-VALUE is a number")
  (:method ((table column-major-table) (column-name symbol) x-value)
    (fit-estimate table (find-column-schema column-name table) x-value)))


