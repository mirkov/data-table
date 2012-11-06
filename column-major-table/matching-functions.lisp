(in-package :numeric-table)

(defun column-matcher (column-schema value table)
  "Return a function of a single argument N-row, the row index.  This
function returns true if the ROW's column value matches VALUE.  This
function is used to search for a value in a specific column

- COLUMN-SCHEMA -- a column schema

- VALUE -- value that we are trying to match.  It is normalized prior
to matching

- TABLE -- the table where we are looking for a match

Based on PCL p. 395"
;;  (declare (ignore type))
  (let* ((n-column (i-column column-schema))
	 (predicate (equality-predicate column-schema))
	 (normalized (normalize-for-column value column-schema)))
    (assert n-column ()
	    "Column index:~a is nil" n-column)
    #'(lambda (N-row)
	(funcall predicate
		 (aref (aref (table table) n-column)
		       N-row)
		 normalized))))

(define-test column-schema-matcher
  (let* ((schema (table-schema *test-table*))
	 (column-schema (find-column-schema 'delta schema))
	 (fun (column-matcher column-schema 3 *test-table*)))
    (assert-true (funcall fun 0))
    (assert-true (not (funcall fun 1)))))

(defmethod column-matchers (table names-and-values)
  "Build a list of column matcher functions for TABLE

NAMES-AND-VALUES is a list of two-element lists.  The elements of the
two-element list are COLUMN-NAME and VALUE.  COLUMN-NAME corresponds
to a column schema name, and value to a value stored in a table.

An example of a call:
 (column-matchers table '((name-1 value-1) (name-2 value-2) ...))
"
  (let ((schema (table-schema table)))
    (loop 
       for (name value) in names-and-values
       when value
       collect (column-matcher (find-column-schema name schema)
			       value table))))

(defun matching-rows (table &rest names-and-values-pairs)
  "Build a WHERE function of index I that returns true when TABLE row
I matches the NAMES-AND-VALUES

NAMES-AND-VALUES-PAIR is a pair of COLUMN-NAME and VALUE.  COLUMN-NAME
corresponds to a column schema name, and value to a value stored in a
table.

An example of a call:
 (matching-rows table '(name-1 value-1) '(name-2 value-2) ...)

PCL p. 395"
  (let ((matchers (column-matchers table names-and-values-pairs)))
    #'(lambda (N-row)
	(every #'(lambda (matcher)
		   (funcall matcher N-row))
	       matchers))))

(define-test matching-rows
  (let ((mf (matching-rows *test-table* '(delta 3) '(lambda-pp 4))))
    (assert-true (funcall mf 0))
    (assert-true (not (funcall mf 1)))))


(defmethod value ((table column-major-table) &key where column-name)
  "Return a table value
- TABLE -- the table
- WHERE -- a function that will return a row index
- COLUMN -- column name, as stored in the schema

The WHERE function is used to find the first matching instance.  

The function returns nil if no value matches the WHERE and COLUMN-NAME"
  (let* ((data (table table))
	 (schema (table-schema table))
	 ;; we now select the matching rows
	 (i-row (loop for i below (row-count table)
		   when (funcall where i)
		   do (return i))))
    (when i-row
      (aref
       (aref data
	     (i-column (find-column-schema column-name schema)))
       i-row))))
  

(define-test value
  (let ((mf (matching-rows *test-table* '(delta 3) '(lambda-pp 4))))
    (assert-number-equal 3 (value *test-table* :where mf :column-name 'delta))
    (assert-number-equal 4 (value *test-table* :where mf :column-name 'lambda-pp)))
  (let ((mf (matching-rows *test-table* '(delta 12))))
    (assert-true (not (value *test-table* :where mf :column-name 'delta)))))
