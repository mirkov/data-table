(in-package :numeric-table)

(defgeneric extractor (table table-schema)
  (:documentation "Return a function of a row index I

This function will return the list of values of TABLE's row I in
columns identified by column-name of the new TABLE-SCHEMA")
  ;;; Modeled after PCL's routine on p. 393
  (:method ((old-table column-major-table) (new-table-schema cons))
    (let ((old-table-schema (table-schema old-table))
	  (data (table old-table)))
    #'(lambda (i-row)
	(mapcar (lambda (column-schema)
		  (aref (aref data
			      (position column-schema old-table-schema
					:key #'name))
			i-row))
		new-table-schema)))))

(defun select (&key (columns t) from where distinct order-by)
  (let ((rows (rows from))
        (schema (schema from)))

    (when where
      (setf rows (restrict-rows rows where)))

    (unless (eql columns 't)
      (setf schema (extract-schema (mklist columns) schema))
      (setf rows (project-columns rows schema)))

    (when distinct
      (setf rows (distinct-rows rows schema)))

    (when order-by
      (setf rows (sorted-rows rows schema (mklist order-by))))

    (make-instance 'table :rows rows :schema schema)))


(defun row-equality-tester (table-schema)
  )




(defun project-columns (rows schema)
  (map 'vector (extractor schema) rows))

(defun distinct-rows (rows schema)
  (remove-duplicates rows :test (row-equality-tester schema)))

(defun sorted-rows (rows schema order-by)
  (sort (copy-seq rows) (row-comparator order-by schema)))
