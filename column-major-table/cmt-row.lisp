(in-package :numeric-table)

(export '(with-column-values row-length column-value))

(defclass column-major-table-row ()
  ((numeric-table :accessor numeric-table
		  :initarg :numeric-table
		  :documentation "Numeric table to whome the row belongs")
   (row-index :accessor row-index
	      :initarg :row-index
	      :documentation
	      "Row index")
   (column-name->index :accessor column-name->index
		       :documentation "Converter of column name to index")
   (table-data-row :accessor table-data-row
		   :documentation
"The nested-vectors object that accesses the row contents")
   )
  (:documentation
"Accessor to a column major table"))

(defmethod print-object ((self column-major-table-row) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "accessing row ~a" (row-index self))))



(defmethod initialize-instance :after ((self column-major-table-row) &key)
  "We create a row accessor for the nested vector"
  (with-slots (numeric-table) self
    (setf (table-data-row self) (nested-vectors:nth-row
				 (table-data numeric-table) (row-index self))
	  (column-name->index self) (column-name->index numeric-table))))

(defgeneric nth-row (numeric-table row-index)
  (:documentation
"Return a numeric-table-row object that enables reading and writing
to elements of row I")
  #+skip(:method :before ((self nested-vector) i)
	   (declare (ignore i))
	   (assert (equal 'operational (state self)) ()
		   "Nested vector state:~a must be OPERATIONAL"
		   (state self)))
  (:method ((self column-major-table) i)
    (make-instance 'column-major-table-row
		   :numeric-table self
		   :row-index i)))

(defmethod column-value ((self column-major-table-row) column-name)
  (vrref (table-data-row self)
	 (funcall (column-name->index self) column-name)))

(defmethod (setf column-value) (value (self column-major-table-row) column-name)
  (setf (vrref (table-data-row self)
	       (funcall (column-name->index self) column-name))
	value))


(defmethod row-length ((self column-major-table-row))
  (column-count (numeric-table self)))

(defmethod row-contents ((self column-major-table-row))
  (row-contents (table-data-row self)))

(defmethod (setf row-index) :after (value (self column-major-table-row))
  "After setting the slot value, I also set the row-index value of the
nested-vector row"
  (setf (row-index (table-data-row self)) value))

(define-test cmt-row
  (let* ((table (loaded-test-table))
	 (row (nth-row table 1)))
    (assert-equal 5 (row-length row))
    (assert-equal 1.3 (value row :column-name 'petal-length))
    (incf (row-index row))
    (assert-equal 3.1 (value row :column-name 'sepal-width))
    (assert-equal 2 (row-index row))
    (setf (value row :column-name 'sepal-length) 12.0)
    (assert-equal 12.0 (vvref (table-data table) 2 0))))

(defun column-bindings (vars column-major-table-row)
  (loop :for v :in vars
       :collect `(,v (column-value ,column-major-table-row ,v))))

(defmacro with-column-values ((&rest vars) row &body body)
  (alexandria:once-only (row)
    `(let ,(column-bindings vars row)
       ,@body)))


