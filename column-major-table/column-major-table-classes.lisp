;; Mirko Vukovic
;; Time-stamp: <2012-11-07 21:27:53Eastern Standard Time column-major-table-classes.lisp>
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

(defmethod make-table-schema ((type (eql 'column-major-table)) spec)
  "Create a table schema by looping over list SPEC

Each SPEC list element is a two element list consisting of the column
name and column type"
  (declare (ignore type))
  (loop :for i from 0
     :for column-spec in spec
     :collect (let ((column-schema (apply #'make-column-schema column-spec)))
		(setf (slot-value column-schema 'i-column) i)
		column-schema)))

(defun test-table-schema ()
  "Build a schema for the test table"
  (make-table-schema
	  'column-major-table
	  (loop
	     :for name in *column-names*
	     :for type in *column-types*
	     :for index from 0
	     collect (list name type
			   :documentation (format nil "column ~a" index)))))

(define-test make-table-schema
  "Test the structure of table schema"
  (let ((schema (test-table-schema)))
    (assert-number-equal 0 (i-column (first schema)))
    (assert-number-equal 1 (i-column (second schema)))
    (assert-number-equal 5 (length schema))))


(defmethod column-documentation ((column-name symbol)
				  (table column-major-table))
  (column-documentation column-name (table-schema table)))

(define-test column-schema
  "Lightly test the contents of column schema"
  (let ((schema (test-table-schema)))
    (assert-equal 'sepal-length (name (first schema)))
    (assert-equal 'number (default-type (first schema)))
    (assert-equal "column 2" (column-documentation 'petal-length schema))))


(defmethod make-table ((type (eql 'column-major-table)) schema
		       &key build-method data-source data-author)
  (let ((table (make-instance type
			      :table-schema schema)))
    (setf (slot-value table 'column-count) (length schema)
	  (slot-value table 'table) (make-array (slot-value table 'column-count)))
    (awhen build-method
      (setf (slot-value table 'build-method) it))
    (awhen data-source
      (setf (data-source table) it))
    (awhen data-author
      (setf (data-author table) it))
    table))

(defun test-column-table ()
  (make-table 'column-major-table (test-table-schema)))

(define-test make-table
  "Lightly test that we can create a table object and load a table
schema"
  (assert-equal 'column-major-table (type-of (test-column-table))))




