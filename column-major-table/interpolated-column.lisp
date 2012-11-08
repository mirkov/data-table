(in-package :numeric-table)


(defclass interpolated-column-schema (column-schema)
  ((interpolation-data
    :accessor interpolation-data
    :initform nil
    :documentation "Data for column-interpolation

In a column-major-table, one column holds the independent variable,
while others are dependent variables.  Each of the dependent columns
holds its interpolation data")
   (independent-var
    :accessor interpolation-data
    :initform nil
    :documentation "Name of column with independent data"))
  (:documentation "Stores interpolation data for a column"))

(defmethod init-column-interp (column-name
			       independent-var-column-name
			       (column-table numeric-table))
  (setf
   (slot-value (column-schema column-name column-table)
	       'interpolation-data)
   (gsll:make-spline gsll:+cubic-spline-interpolation+
		     (aref column-table (position independent-var-column-name
						  (table-schema column-table)))
		     (aref column-table (position column-name
						  (table-schema column-table))))
   (slot-value (table-column column-name column-table)
	       'independent-var)
   independent-var-column-name))
  

