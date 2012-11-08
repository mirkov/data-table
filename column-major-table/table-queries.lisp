(in-package :numeric-table)

(defgeneric extractor (table table-schema)
  (:documentation "Return a function of a row index I

This function will return the list of values of TABLE's row I in
columns identified by column-name of the new TABLE-SCHEMA")
;;; Modeled after PCL's routine on p. 393
  (:method ((old-table column-major-table) (table-schema cons))
    (let* ((table-schema/old (table-schema old-table))
	   (data (table old-table))
	   (columns (mapcar
		     (lambda (column-schema/new)
		       (position-if
			(lambda (column-schema/old)
			  (equal (name column-schema/new)
				 (name column-schema/old)))
			table-schema/old))
		     table-schema)))
      #'(lambda (i-row)
	  (mapcar (lambda (i-column)
		    (vvref data i-row i-column))
		  columns)))))

(define-test extractor
  (let* ((old-table (loaded-test-table))
	 (old-schema (table-schema old-table))
	 (new-schema (list (second old-schema)
			   (fourth old-schema)))
	 (extractor (extractor old-table new-schema)))
    (assert-numerical-equal '(3.6 0.2) (funcall extractor 3))))


(defun find-column (column-name table-schema)
  (or (find column-name table-schema :key #'name)
      (error "No column: ~a in schema" column-name)))

(defun extract-schema (column-names schema)
  (loop :for c in column-names
     :collect (find-column c schema)))

(defmethod restrict-rows (data where)
  (remove-if-not data where))

(defun project-columns (data schema))

(defun select (&key (columns t) from where distinct order-by)
  (let ((data (table from))
        (schema (schema from)))

    (when where
      (setf data (restrict-rows data where)))

    (unless (eql columns 't)
      (setf schema (extract-schema (mklist columns) schema))
      (setf data (project-columns data schema)))

    (make-instance 'column-major-table :table data :table-schema schema)))


(defun project-columns (rows schema)
  (map 'vector (extractor schema) rows))

