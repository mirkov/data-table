(in-package :numeric-table)

(export '(matching-rows in empty-p not-empty-p))


(defgeneric column-matcher (column-schema value-or-test &optional predicate)
  (:documentation 
   "Return a function of a single argument, the n-th row.  This
function returns true if the ROW's column value matches VALUE or satisfies 
FUNCTION returns T.
")
  (:method ((column-schema column-schema) value &optional predicate)
    "Return a function of a single argument N-row, the row index.  This
function returns true if the ROW's column value matches VALUE.

- COLUMN-SCHEMA -- a column schema

- VALUE -- value that we are trying to match.  It is normalized prior
to matching

- TABLE -- the table where we are looking for a match"
    ;;Based on PCL p. 395
    (let* ((n-column (slot-value column-schema 'i-column))
	   (predicate
	    (aif predicate it
		 (slot-value column-schema 'equality-predicate)))
	   (normalized (normalize-value value column-schema)))
      (assert n-column ()
	      "Column index:~a is nil" n-column)
      #'(lambda (row)
	  (funcall predicate
		   (vrref row n-column)
		   normalized))))
  (:method ((column-schema column-schema) (test (eql 'empty-p))
	    &optional predicate)
    "Return a function of a single argument N-row, the row index.  This
function returns true if the columns value equals the empty value"
    (declare (ignore dummy-arg))
    (let* ((n-column (slot-value column-schema 'i-column))
	   (predicate
	    (aif predicate it
		 (slot-value column-schema 'equality-predicate)))
	   (normalized-empty-value
	    (funcall (value-normalizer column-schema)
		     (empty-value column-schema)
		     column-schema)))
      (assert n-column ()
	      "Column index:~a is nil" n-column)
      #'(lambda (row)
	  (funcall predicate
		   (vrref row n-column)
		   normalized-empty-value))))
  (:method ((column-schema column-schema) (test (eql 'not-empty-p))
	    &optional predicate)
    "Return a function of a single argument N-row, the row index.
This function returns true if the columns value does not equal the
empty value"
    (declare (ignore dummy-arg))
    (let ((predicate (column-matcher column-schema 'empty-p predicate)))
      #'(lambda (N-row)
	  (not (funcall predicate N-row))))))

(define-test column-schema-matcher
  (let* ((table (loaded-test-table))
	 (schema (table-schema table))
	 (column-schema (find-column-schema 'sepal-length schema))
	 (fun (column-matcher column-schema 4.7)))
    (assert-true (funcall fun (nested-vectors:nth-row (table-data table)
						      1)))
    (assert-true (not (funcall fun (nested-vectors:nth-row (table-data table)
							   2))))))

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
       for (name value . predicate-maybe) in names-and-values
       when value
       collect (column-matcher (find-column-schema name schema)
			       value (car predicate-maybe)))))

(defun matching-rows (table &rest names-and-values-pairs)
  "Build a WHERE function of row index I that returns true when TABLE row
I matches the NAMES-AND-VALUES/FUNCTIONS.

NAMES-AND-VALUES-PAIR is a pair of COLUMN-NAME and VALUE or FUNCTION.
COLUMN-NAME corresponds to a column schema name, and value to a value
stored in a table.

An example of a call:
 (matching-rows table '(name-1 value-1) '(name-2 value-2) ...)

Matching

PCL p. 395"
  (let ((matchers (column-matchers table names-and-values-pairs)))
    #'(lambda (N-row)
	(every #'(lambda (matcher)
		   (funcall matcher N-row))
	       matchers))))

(define-test matching-rows
  "We test for rows that match sepal length of 5.4 and petal width of 0.4

Rows 4 and 9 have match sepal length, but only row 4 matches petal width"
  (let* ((table (loaded-test-table))
	 (data (table-data table))
	 (mf (matching-rows table '(sepal-length 5.4) '(petal-width 0.4))))
    (assert-true  (funcall mf (nested-vectors:nth-row data 4)))
    (assert-true (not (funcall mf (nested-vectors:nth-row data 9))))))


(defmethod value ((table column-major-table) &key where column-name)
  "Return a table value
- TABLE -- the table
- WHERE -- a function that will return a row index
- COLUMN -- column name, as stored in the schema

The WHERE function is used to find the first matching instance.  

The function returns nil if no value matches the WHERE and COLUMN-NAME"
  (let* ((data (table-data table))
	 (schema (table-schema table))
	 (column-schema (find-column-schema column-name schema))
	 (row
	  (iter:iter
	    (iter:for row :in-nv-row data)
	    (when (funcall where row)
	      (return row)))))
    (when row
      (vrref row (i-column column-schema)))))
  

(defun in (column source-table query-table &optional predicate)
  "Build a WHERE function of row that returns true when the
value in COLUMN of QUERY-TABLE is found in COLUMN of SOURCE-TABLE"
  ;; Offers the same functionality as PCL's In function (p. 396).  But
  ;; the structure is completely different.  The interface is also
  ;; different.
  (let ((target-schema (find-column-schema column query-table)))
    (unless predicate
      (setf predicate
	    (equality-predicate target-schema)))
    (let ((j-target (i-column target-schema)))
      (lambda (row)
	(let ((target-value (vrref row j-target)))
	  ;; Use iter instead of `find' or `loop' in order to
	  ;; accomodate grids as well as arrays.
	  (iter:iter
	    (iter:for element :vector-element (table-column column source-table))
	    (when (funcall predicate target-value element)
	      (return t))
	    (iter:finally (return nil))))))))

(define-test value
  (let ((table (loaded-test-table)))
    (let ((mf (matching-rows  table '(sepal-length 5.4) '(petal-width 0.4))))
      (assert-number-equal 3.9 (value table :where mf :column-name 'sepal-width))
      (assert-number-equal 1.7 (value table :where mf :column-name 'petal-length)))
    (let ((mf (matching-rows  table '(sepal-length 5.4) '(petal-width 99))))
      (assert-true (not (value table :where mf :column-name 'sepal-width))))))


(defmethod (setf value) (value (table column-major-table) &key where column-name)
  "Set a table value"
  (let* ((data (table-data table))
	 (schema (table-schema table))
	 (column-schema (find-column-schema column-name schema))
	 ;; we now select the matching rows
	 (row (iter:iter
		(iter:for row :in-nv-row data)
		   (when (funcall where row)
		     (return row)))))
    (when row
      (setf (vrref row (i-column column-schema))
	    (funcall (value-normalizer column-schema) value column-schema)))))


(define-test setf-value
  (let ((table (loaded-test-table)))
    (let ((mf (matching-rows  table '(sepal-length 5.4) '(petal-width 0.4))))
      (assert-number-equal 3.9 (value table :where mf :column-name 'sepal-width))
      (assert-number-equal 1.7 (value table :where mf :column-name 'petal-length))
      (setf (value table :where mf :column-name 'sepal-width) 3.85)
      (assert-number-equal 3.85 (value table :where mf :column-name 'sepal-width)))))

