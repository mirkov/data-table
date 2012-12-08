;; Mirko Vukovic
;; Time-stamp: <2012-11-22 10:13:07Eastern Standard Time generic-numeric-table.lisp>
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
   (row-count :reader row-count
	      :initform 0
	      :documentation "Return number of rows
In case of variable row count return values 'variable and max-row-count")
   (column-count :reader column-count
		 :documentation "Return number of columns
In case of a variable column count, return values 'variable and max-column-count"
		 :initform 0)
   (table-schema :accessor table-schema :initarg :table-schema
		 :documentation "Stores table meta-data 

- Column names
- Functions for setting and comparing column values")
   (build-method :reader build-method
		 :initform nil
		 :documentation "A symbol specifying the build method")
   (data-source :accessor data-source
		:documentation "The physical origin of the data

It can be a file, a computation, interactive, or something else")
   (data-author :accessor data-author
		:documentation "Who were the people that created the data

This can be a journal reference or something else"))
  (:documentation "Stores a numeric table"))

(defgeneric make-table (type table-schema &key build-method
			     data-source data-author
			     &allow-other-keys)
  (:documentation "Make a table of given TYPE and specify the
TABLE-SCHEMA.  Optionally specify BUILD-METHOD, DATA-SOURCE,
DATA-AUTHOR.

Methods may add additional keys"))

(defgeneric table-column (name table)
  (:documentation "Return the column data (not the column schema) from
  TABLE using column NAME"))

(defgeneric nth-column (n table)
  (:documentation "Return the N-th column of TABLE"))

(defgeneric set-table-column (column-id table column-vector
				    &key overwrite
				    &allow-other-keys)
  (:documentation 
"Load COLUMN-VECTOR into TABLE column specified by COLUMN-ID

COLUMN-ID can be the column name or column index

OVERWRITE, if T, allows an existing column to be overwritten"))

(defsetf table-column set-table-column
    "Set table column using SET-TABLE-COLUMN")


(defgeneric insert-row (row table)
  (:documentation "Add ROW to the TABLE"))


(defgeneric nth-row (N table)
  (:documentation "Return at N-th TABLE row"))

(defgeneric table-size (numeric-table)
  (:documentation "Return the table size of a rectangular table
For some tables, that have variable length rows or columns, the table
size might not be meaningful"))

;;; Schema functionality
(defgeneric make-table-schema (type spec)
  (:documentation "Create TABLE-SCHEMA based on SPEC for table TYPE"))

(defclass column-schema ()
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
				   v))
   (default-value :reader default-value
     :initarg :default-value
     :initform nil)
   (default-type :reader default-type
     :initarg :default-type
     :initform nil
     :documentation "Stored values will be compared against the type")
   (i-column :reader i-column
	     :initarg :i-column
	     :initform nil
	     :documentation "Stores the column index")
   (documentation :initarg :documentation
		  :initform nil
		  :reader column-documentation%
		  :documentation "Text describing the documentation")
   (empty-value :initarg :empty-value
		:initform nil
		:reader empty-value
		:documentation "Value that signifies an empty cell")
   )
  (:documentation "Store the schema for a column"))

(defgeneric make-column-schema (name type
			      &key comparator equality-predicate
			      default-type documentation empty-value
			      &allow-other-keys)
  (:documentation "Make instance of COLUMN-SCHEMA object with NAME and
  specialized to hold data of TYPE

If TYPE is 'custom, then keywords COMPARATOR, EQUALITY-PREDICATE,
DEFAULT-TYPE must be used to specify the column:

COMPARATOR and EQUALITY-PREDICATE must be functions that accept two
arguments and return t or nil

DEFAULT-TYPE must be a type specification that will be passed to TYPEP.

EMPTY-VALUE is the value that signifies an unspecified cell
")
  (:method  (name (type (eql 'string)) &key documentation &allow-other-keys)
    (make-instance 'column-schema
		   :name name
		   :comparator #'string<
		   :equality-predicate #'string=
		   :default-type 'string
		   :documentation documentation))
  (:method  (name (type (eql 'number)) &key documentation &allow-other-keys)
    (make-instance 'column-schema
		   :name name
		   :comparator #'<
		   :equality-predicate #'=
		   :default-type 'number
		   :documentation documentation))
  (:method (name (type (eql 'symbol)) &key documentation &allow-other-keys)
    (make-instance 'column-schema
		   :name name
		   :comparator #'(lambda (arg1 arg2)
				   (string<  arg1
					     arg2))
		   :equality-predicate #'eq
		   :default-type 'symbol
		   :documentation documentation))
  (:method (name (type (eql 'custom)) &key comparator equality-predicate default-type documentation empty-value)
    (make-instance 'column-schema
		   :name name
		   :comparator comparator
		   :equality-predicate equality-predicate
		   :default-type default-type
		   :empty-value empty-value
		   :documentation documentation)))

;;; Loading of value-normalizer.  By default they check that the value
;;; is of correct type, as specified by default type
;;;
;;; I initialize it only after default-type is loaded.
(defmethod initialize-instance :after ((self (eql 'string)) &key)
  (with-slots (value-normalizer default-type) self
    (setf value-normalizer
	  #'(lambda (value column-schema)
	      (declare (ignore column-schema))
	      (assert (typep value default-type))))))
(defmethod initialize-instance :after ((self (eql 'number)) &key)
  (with-slots (value-normalizer default-type) self
  (setf value-normalizer
	#'(lambda (value column-schema)
	    (declare (ignore column-schema))
	    (assert (typep value default-type))))))
(defmethod initialize-instance :after ((self (eql 'symbol)) &key)
  (with-slots (value-normalizer default-type) self
  (setf value-normalizer
	#'(lambda (value column-schema)
	    (declare (ignore column-schema))
	    (assert (typep value default-type))))))
(defmethod initialize-instance :after ((self (eql 'index)) &key)
  (with-slots (value-normalizer default-type) self
  (setf value-normalizer
	#'(lambda (value column-schema)
	    (declare (ignore column-schema))
	    (assert (typep value default-type))))))

(defgeneric normalize-value (value column-schema)
  (:documentation "Normalize VALUE according to COLUMN-SCHEMA")
  (:method (value column-schema)
    (funcall (slot-value column-schema 'value-normalizer)
	     value column-schema)))

(defun normalize-for-column (value column-schema)
  "Normalize VALUE according to specifications in COLUMN-SCHEMA"
  (funcall (value-normalizer column-schema) value column-schema))

(defgeneric column-names (table/table-schema)
  (:documentation "Return list of column names from table or table schema")
  (:method ((table-schema cons))
    (mapcar #'column-name table-schema))
  (:method ((table numeric-table))
    (column-names (table-schema table))))

(defgeneric column-documentation (column-name object)
  (:documentation "Return column documentation from OBJECT
COLUMN-NAME is the name of the column")
  (:method ((column-name symbol) (schema cons))
    (column-documentation% (find-column-schema column-name schema))))


(defgeneric find-column-schema (name container)
  (:documentation "Return COLUMN-SCHEMA by NAME from CONTAINER

NAME is a symbol by which the column is identified in the TABLE-SCHEMA
CONTAINER can be either a table, or a table-schema")
  (:method ((name symbol) (container numeric-table))
    (find name (table-schema container) :key #'name))
  (:method ((name symbol) (container cons))
    (find name container :key #'name)))

(defun extract-column-schema (column-names table-schema)
  (loop for c in column-names
       collect (find-column c table-schema)))

(defgeneric restrict-rows (table where)
  (:documentation "Return a new table with rows that satisfy WHERE
TABLE is the table containing the original rows

Particular methods can take considerable latitutude in how they will
implement this method.  Read their documentation"))


(defgeneric select (table &key columns where distinct order-by)
  (:documentation
"Return from TABLE rows that match criteria specified by the WHERE function
Return only columns with names listed in COLUMNS.

If COLUMNS is T, return all columns
If WHERE is NIL, return all rows

The returned table can be sorted columns listed in ORDER-BY
If DISTINCT is T, eliminate duplicate rows from results

Methods specializing on different table types need not implement all
of the selection criterial.  Consult the documentation for each
method.
"))


(defgeneric value (table &key where column-name &allow-other-keys)
  (:documentation 
"Return a TABLE value.

WHERE is a function of ROW and returns T or a generalized boolean for
the desired row.

COLUMN-NAME is a column name as stored in the schema"))
