(in-package :numeric-table)

(export '(col-independent-var independent-var))

(defclass column-fit ()
  ((independent-var
    :accessor col-independent-var
    :initarg :independent-var
    :initform nil
    :documentation "Name of column with independent data")
   (sigma
    :accessor sigma
    :initarg sigma
    :initform 1d0
    :documentation "Name of column or a double-float specifying the sigma
Initially set to 1d0")
   (chi^2
    :reader chi^2
    :initform nil
    :documentation "chi^2 of the fit"))
  (:documentation "Base class for fitting"))

(defclass linear-column-fit (column-fit)
  ((fit-coeffs
    :reader fit-coeffs
    :initform nil
    :documentation "List of fitting coefficients")
   (covariance
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


(defgeneric (setf independent-var) (x-col table y-col)
  (:documentation
   "Set Y-COL's independent-variable to X-COL

TABLE - a column major table
Y-COL and X-COL are either names or schemas")
  (:method ((x-col symbol) (table column-major-table) (y-col symbol))
    (setf (col-independent-var (find-column-schema y-col table)) x-col))
  (:method ((x-col symbol) (table column-major-table) (y-col column-schema))
    (setf (col-independent-var y-col) x-col))
  (:method ((x-col column-schema) (table column-major-table) (y-col symbol))
    (setf (col-independent-var (find-column-schema y-col table)) (column-name x-col)))
  (:method ((x-col column-schema) (table column-major-table) (y-col column-schema))
    (setf (col-independent-var y-col) (column-name x-col))))
