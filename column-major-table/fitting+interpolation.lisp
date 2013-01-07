(in-package :numeric-table)

(export '(col-independent-var evaluate))


(defclass column-fit+interpolation ()
  ((independent-var
    :accessor col-independent-var
    :initarg :independent-var
    :initform nil
    :documentation "Name of column with independent data")
   (method
    :initarg :method
    :documentation "Stores the column interpolation or fitting method

This slot's definition is updated by inherited classes"))
  (:documentation
"Base class for column fitting and interpolation"))


(defmethod describe-object :after ((self column-fit+interpolation) stream)
  (format stream "Independent variable: ~a~%" (col-independent-var self)))

(defgeneric evaluate (table column x-value)
  (:documentation
"Evaluate a column fit or interpolation for x-value")
  (:method ((table column-major-table) (name symbol) x-value)
    (evaluate table (find-column-schema name table) x-value)))

(defgeneric independent-var (table y-col)
  (:documentation 
"Retreive Y_COL's independent variable")
  (:method ((table column-major-table) (y-col symbol))
    (col-independent-var (find-column-schema y-col table))))

(defgeneric (setf independent-var) (x-col table y-col)
  (:documentation
   "Set Y-COL's independent-variable to X-COL

TABLE - a column major table
Y-COL and X-COL are either names or schemas")
  (:method ((x-col symbol) (table column-major-table) (y-col symbol))
    (setf (col-independent-var (find-column-schema y-col table)) x-col))
  (:method ((x-col column-schema) (table column-major-table) (y-col symbol))
    (setf (col-independent-var (find-column-schema y-col table)) (column-name x-col))))
