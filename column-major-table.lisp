;; Mirko Vukovic
;; Time-stamp: <2012-08-02 13:44:19 column-major-table.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :numeric-table)

;;; Classes and methods for instantiating column-major tables
(defclass column-major-table (numeric-table)
  ()
  (:documentation "Table that contains multiple columns

The table can be expanded by adding both rows and columns"))

(define-test instantiate-column-major-table
  (assert-true (make-instance 'column-major-table)))

(defmethod table-size ((table column-major-table))
  (list (row-count table) (column-count table)))

(defmethod make-schema ((type (eql 'column-major-table)) spec)
  "Create a table schema by looping over list SPEC

Each SPEC list element is a two element list consisting of the column
name and column type"
  (declare (ignore type))
  (loop :for i from 0
     :for column-spec in spec
     :collect (let ((column (apply #'make-column column-spec)))
	       (setf (slot-value column 'i-column) i)
	       column)))

(define-test make-schema
  (let ((schema
	 (make-schema 'column-major-table '((delta number) (lambda-pp number)))))
    (assert-number-equal 0 (i-column (first schema)))
    (assert-number-equal 1 (i-column (second schema)))
    (assert-equal 'delta (name (first schema)))))


(defgeneric column-names (object)
  (:documentation "Return list of column names.  OBJECT can be a table
  or a schema")
  (:method ((schema cons))
    (loop :for column-def in schema
	 :collect (name column-def)))
  (:method ((table column-major-table))
    (column-names (schema table))))

(defmethod column-documentation ((column-name symbol)
				  (table column-major-table))
  (column-documentation column-name (schema table)))

(defmethod make-table ((type (eql 'column-major-table)) schema
		       &key build-method data-source data-author)
  (let ((table (make-instance type
			      :schema schema)))
    (setf (slot-value table 'column-count) (length schema))
    (awhen build-method
      (setf (slot-value table 'build-method) it))
    (awhen data-source
      (setf (data-source table) it))
    (awhen data-author
      (setf (data-author table) it))
    table))


(define-test make-table
  (assert-true (make-table 'column-major-table
			   (make-schema 'column-major-table
					'((delta number) (lambda-pp number))))))


;;; methods for row insertion
(defmethod insert-row ((row list) (table column-major-table))
  (with-slots (build-method table column-count row-count schema) table
    (assert schema ()
	    "The table must have a schema")
    (cond 
      ((null build-method)
       (setf build-method 'row-by-row
	     table (make-array column-count))
       (dotimes (i column-count)
	 (setf (aref table i) (make-array 0 :adjustable t :fill-pointer 0))))
      ((eql build-method 'row-by-row))
      (t 
       (error "Table build-method is not defined as row-by-row.  It is ~a"
	      build-method)))
			     
    (loop
       :for value in row
       :for table-column across table
       :for column-schema in schema
       :for column-index from 0
       :do (vector-push-extend (normalize-value value column-index column-schema)
			     table-column))
    (incf row-count)))


(defmethod nth-row ((n integer) (table column-major-table))
  (with-slots (row-count column-count table) table
      (assert (< n row-count) ()
	      "Row index ~a exceeds row-count ~a" n row-count)
    (let ((row (make-array column-count)))
      (loop :for column across table
	 :for i from 0
	 :do (setf (aref row i) (aref column n)))
      row)))

(defmacro with-bare-test-table (&body body)
  `(let ((test-table
	 (make-table 'column-major-table
		     (make-schema 'column-major-table
				  '((delta number) (lambda-pp number))))))
     ,@body))

(define-test insert-row
  (with-bare-test-table
    (assert-number-equal 2 (column-count test-table))
    (insert-row '(3 4) test-table)
    (assert-number-equal 1 (row-count test-table))
    (assert-numerical-equal #(3 4)
			    (table-row 0 test-table))))

(defparameter *test-table*
  (with-bare-test-table
    (insert-row '(3 4) test-table)
    (insert-row '(13 14) test-table)
    test-table))

(define-test *test-table*
  (first (schema *test-table*)))


(defmethod nth-column ((n integer) (table column-major-table))
  (with-slots (column-count table) table
  (assert (< n column-count) ()
	  "Column index ~a is greater than number of columns ~a"
	  n column-count)
  (aref table n)))

(define-test table-column
  (with-bare-test-table
    (insert-row '(3 4) test-table)
    (insert-row '(8 5) test-table)
    (assert-number-equal 2 (row-count test-table))
    (assert-numerical-equal #(3 4)
			    (table-row 0 test-table))
    (assert-numerical-equal #(8 5)
			    (table-row 1 test-table))
    (assert-numerical-equal #(3 8)
			    (table-column 0 test-table))))


(defmethod table-column ((column-name symbol)
			 (table column-major-table))
  "Return column contents of column matching COLUMN-NAME"
  (nth-column
   (position column-name (schema table)
	     :key #'name)
   table))
#|
(defmethod set-table-column ((table column-major-table)
			     (column-index integer)
			     (column-vector array)
			     &key (overwrite nil))
  (assert (< column-index (column-count table)) ()
	  "Column index ~a is greater than number of columns ~a"
	  column-index (column-count table))
  (when
   (and (not (null (aref (table table) column-index) ))
	(not overwrite))
   (error "Attempting to overwrite column ~a" column-index))
  (if (row-count table)
    (and (assert (= (length column-vector) (row-count table)) ()
	    "The new column length ~a does not match table row count ~a"
	    (length column-vector) (row-count table))
	 (setf table-column (length column-vector)))
    (setf (row-count table) (length column-vector)
	  (aref (table table) column-index) column-vector)))
|#


;;; Selection and value return functions modeled after PCL 392


#|
;;; Code for selection
(defun mklist (thing)
  (if (listp thing) thing (list thing)))


(defun extractor ())

(defun project-columns (rows schema table)
  (map 'vector (extractor schema) rows))|#





(defun column-matcher (column value table)
  "Return a function of a single argument N-row, the row index.  This
  function returns true if the ROW's column value matches VALUE.

- COLUMN -- a column schema

- VALUE -- value that we are trying to match.  It is normalized prior
to matching

- TABLE -- the table where we are looking for a match

Based on PCL p. 395"
;;  (declare (ignore type))
  (let* ((n-column (i-column column))
	 (predicate (equality-predicate column))
	 (normalized (normalize-for-column value column)))
    (assert n-column ()
	    "Column index:~a is nil" n-column)
    #'(lambda (N-row)
	(funcall predicate
		 (aref (aref (table table) n-column)
		       N-row)
		 normalized))))

(define-test column-matcher
  (let* ((schema (schema *test-table*))
	 (column (find-column 'delta schema))
	 (fun (column-matcher column 3 *test-table*)))
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
  (let ((schema (schema table)))
    (loop 
       for (name value) in names-and-values
       when value
       collect (column-matcher (find-column name schema) value table))))

(defun matching-rows (table &rest names-and-values-pairs)
  "Build a WHERE function that returns true when a TABLE row matches the
  NAMES-AND-VALUES

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


(defmethod value ((from column-major-table) &key where column-name)
  "Return a table value
- FROM -- the table
- WHERE -- a function that will return a row index
- COLUMN -- column name, as stored in the schema

The WHERE function is used to find the first matching instance.  

The function returns nil if no value matches the WHERE and COLUMN-NAME"
  (let* ((table (table from))
	 (schema (schema from))
	 ;; we now select the matching rows
	 (i-row (loop for i below (row-count from)
		   when (funcall where i)
		   do (return i))))
    (when i-row
      (aref
       (aref table
	     (i-column (find-column column-name schema)))
       i-row))))
  

(define-test value
  (let ((mf (matching-rows *test-table* '(delta 3) '(lambda-pp 4))))
    (assert-number-equal 3 (value *test-table* :where mf :column-name 'delta))
    (assert-number-equal 4 (value *test-table* :where mf :column-name 'lambda-pp)))
  (let ((mf (matching-rows *test-table* '(delta 12))))
    (assert-true (not (value *test-table* :where mf :column-name 'delta)))))
  