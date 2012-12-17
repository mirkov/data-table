(in-package :numeric-table)

(export '(read-table))

(defmethod read-table (stream (table column-major-table))
  (let ((columns (column-count table)))
    (loop :for line = (read-line stream nil 'eof nil)
       :until (eq line 'eof)
       :do (insert-row
	    (loop :for i-column below columns
	       :with position = 0
	       :collect (multiple-value-bind (value position%)
			    (read-from-string line t nil :start position)
			  (setf position position%)
			  value))
	    table))))
