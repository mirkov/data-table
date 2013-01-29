(in-package :numeric-table)

(export '(read-table write-table))

(defmethod read-table (stream (table column-major-table))
  "Read table contents from stream"
  (let ((columns (column-count table)))
    (loop :for line = (read-line stream nil 'eof nil)
       :until (eq line 'eof)
       :do (add-row
	    (loop :for i-column below columns
	       :with position = 0
	       :collect (multiple-value-bind (value position%)
			    (read-from-string line t nil :start position)
			  (setf position position%)
			  value))
	    table))))

(defmethod read-table :after (stream (table column-major-table))
  "Following reading in a table, coerce all vectors to correct grid
type (native cl:array or grid:foreign-array) "
  (declare (ignore stream))
  (setf (nested-vectors:adjustable-row-count
	 (table-data table)) nil)
  (coerce-vectors-grid-type table))

(defun write-table (stream table)
  (dotimes (j (column-count table))
    (cl-csv:write-csv-row
     (loop for i below (row-count table)
	collect (vvref (table-data table) i j))
     :stream stream)))
