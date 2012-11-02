;; Mirko Vukovic
;; Time-stamp: <2012-04-23 23:01:40 numeric-table.lisp>
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

(defclass numeric-table ()
  ((table :accessor table :initarg :table)
   (rows :reader rows
	 :initform 0
	 :documentation "Return number of rows")
   (columns :reader columns
	    :documentation "Return number of columns"
	    :initform 0)
   (schema :accessor :schema :initarg :schema
	   :documentation "Stores table meta-data 

- Column names
- Functions for setting and comparing column values")))

(defgeneric make-table (type schema)
  (:documentation "Make a table of given TYPE and specify the SCHEMA"))

(defclass 1d-columns (numeric-table)
  ((table :initform (make-array 0 :adjustable t
				:fill-pointer 0)))
  (:documentation "Table that contains multiple columns

The table can be expanded by adding both rows and columns"))

(defclass 2d-table (numeric-table)
  ())

(defclass column ()
  ((name :reader name
	 :initarg :name)
   (equality-predicate :reader equality-predicate
		       :initarg :equality-predicate
		       :initform #'=)
   (comparator :reader comparator
	       :initarg :comparator
	       :initform #'>)
   (value-normalizer :reader value-normalizer
		     :initarg :value-normalizer
		     :initform #'(lambda (v column)
				   (declare (ignore column))
				   v))))

(defgeneric make-column (name type)
  (:documentation "Make instance of COLUMN object with NAME and
  specialized to hold data of TYPE")
  (:method  (name (type (eql 'string)))
    (make-instance 'column
		   :name name
		   :comparator #'string<
		   :equality-predicate #'string=))
  (:method  (name (type (eql 'number)))
    (make-instance 'column
		   :name name
		   :comparator #'<
		   :equality-predicate #'=))
  (:method (name (type (eql 'symbol)))
    (make-instance 'column
		   :name name
		   :comparator #'(lambda (arg1 arg2)
				   (string< (symbol-name arg1)
					    (symbol-name arg2)))
		   :equality-predicate #'eq)))

(defun make-schema (spec)
  "Create a table schema by looping over list SPEC

Each SPEC list element is a two element list consisting of the column
name and column type"
  (mapcar #'(lambda (column-spec)
	      (apply #'make-column column-spec))
	  spec))

(defmethod make-table (type schema)
  (let ((table (make-instance type
			      :schema schema)))
    (setf (columns table) (length schema))))

(defgeneric table-column (table column-id)
  (:documentation "Return column from TABLE using COLUMN-ID"))

(defmethod table-column ((table 1d-columns) (column-index integer))
  (assert (< column-id (columns table) ()
	     "Column index ~a is greater than number of columns ~a"
	     column-index (columns table)))
  (aref table column-index))

(defgeneric set-table-column (table column-id column-vector
				    &key overwrite)
  (:documentation 
"Load COLUMN-VECTOR into TABLE column specified by COLUMN-ID

OVERWRITE, if T, allows an existing column to be overwritten"))


(defmethod set-table-column ((table 1d-columns)
			     (column-index integer)
			     (column-vector array)
			     &key (ovewrite nil))
  )
(defsetf table-columns (table column-index column-data))