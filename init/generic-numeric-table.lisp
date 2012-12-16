(in-package :numeric-table)


(export '(numeric-table
	  row-count column-count
	  table-schema
	  build-method data-source data-author
	  make-table table-column set-table-column
	  nth-column set-nth-column
	  insert-row nth-row
	  table-size
	  ;;
	  make-table-schema
	  make-column-schema
	  normalize-for-column
	  ;;
	  column-names column-doc
	  find-column-schema nth-column-schema
	  ;;
	  select value
	  ))

(defclass numeric-table ()
  ((table :initarg :table
	  :documentation "Table data")
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

(defgeneric set-table-column (column-name table column-vector
				    &key overwrite
				    &allow-other-keys)
  (:documentation 
"Load COLUMN-VECTOR into TABLE column specified by COLUMN-NAME

OVERWRITE, if T, allows an existing column to be overwritten"))

(defsetf table-column set-table-column
    "Set table column using SET-TABLE-COLUMN")

(defgeneric nth-column (n table)
  (:documentation "Return the N-th column of TABLE"))

(defgeneric set-nth-column (n table column-vector
			    &key overwrite
			      &allow-other-keys)
  (:documentation"Load COLUMN-VECTOR into TABLE's N-th column.

OVERWRITE, if T, allows an existing column to be overwritten"))



(defgeneric insert-row (row table)
  (:documentation "Add ROW to the TABLE

Row is a vector of list of values.  Its length matches tables number
of columns.
"))


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
  ((name ;;:reader name
	 :initarg :name)
   (equality-predicate ;;:reader equality-predicate
		       :initarg :equality-predicate
		       :initform #'=)
   (comparator ;;:reader comparator
	       :initarg :comparator
	       :initform #'>)
   (value-normalizer ;;:reader value-normalizer
		     :initarg :value-normalizer
		     :initform #'(lambda (v column)
				   (declare (ignore column))
				   v))
   (default-value ;;:reader default-value
     :initarg :default-value
     :initform nil)
   (default-type ;;:reader default-type
     :initarg :default-type
     :initform nil
     :documentation "Stored values will be compared against the type")
   (i-column ;;:reader i-column
	     :initarg :i-column
	     :initform nil
	     :documentation "Stores the column index")
   (documentation :initarg :documentation
		  :initform nil
		  :reader column-doc
		  :documentation "Text describing the documentation")
   (empty-value :initarg :empty-value
		:initform nil
		;;:reader empty-value
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

;;; Loading of value-normalizer.  By default it checks that the value
;;; is of correct type, as specified by default type
;;;
;;; I thus initialize it only after default-type is loaded.
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

(defgeneric normalize-value (value schema)
  (:documentation "Normalize VALUE according to SCHEMA

SCHEMA can be a COLUMN-SCHEMA or (when implemented) ROW-SCHEMA")
  (:method (value (column-schema column-schema))
    (funcall (slot-value column-schema 'value-normalizer)
	     value column-schema)))

(defgeneric column-names (table/table-schema)
  (:documentation "Return list of column names from table or table schema")
  (:method ((table-schema cons))
    (mapcar #'column-name table-schema))
  (:method ((table numeric-table))
    (column-names (table-schema table))))

(defgeneric column-documentation (column-name table-or-table-schema)
  (:documentation "Return column documentation from TABLE or TABLE-SCHEMA")
  (:method ((column-name symbol) (table-schema cons))
    (column-doc% (find-column-schema column-name table-schema)))
  (:method ((column-name symbol) (table numeric-table))
    (column-doc% (find-column-schema column-name 
				     (table-schema table)))))


(defgeneric find-column-schema (column-name table-or-table-schema)
  (:documentation "Return COLUMN-SCHEMA by COLUMN-NAME from either a
  TABLE or its TABLE-SCHEMA

NAME is a symbol by which the column is identified in the TABLE-SCHEMA
CONTAINER can be either a table, or a table-schema")
  (:method ((name symbol) (table numeric-table))
    (find name (table-schema table) :key #'name))
  (:method ((name symbol) (table-schema cons))
    (find name table-schema :key #'name)))

(defgeneric nth-column-schema (n table-or-table-schema)
  (:documentation "Return COLUMN-SCHEMA by position from TABLE or
  TABLE-SCHEMA

N is an integer
CONTAINER can be either a table, or a table-schema")
  (:method ((n integer) (table numeric-table))
    (nth n (table-schema table)))
  (:method ((n integer) (table-schema cons))
    (nth n table-schema)))

(defgeneric collect-column-schema (column-names table-or-table-schema)
  (:documentation "Collect all column-schemas in TABLE or TABLE-SCHEMA

COLUMN-NAMES is a list of column names")
  (:method ((column-names cons) (table numeric-table))
    (collect-column-schema column-names (table-schema table)))
  (:method ((column-names cons) (table-schema cons))
    (loop for c in column-names
       collect (find-column c table-schema))))

(defgeneric restrict-rows (table where)
  (:documentation "Return a new table whose rows satisfy WHERE.

TABLE is the original table

WHERE is a function that is applied to each row in turn.  The function
argument depends on the table type.  See the documentation for each
particular method."))


(defgeneric select (table &key columns where distinct order-by)
  (:documentation
"Return from TABLE rows that match criteria specified by the WHERE function
Return only columns with names listed in COLUMNS.

If COLUMNS is T, return all columns
If WHERE is NIL, return all rows

The returned table can be sorted column values listed in ORDER-BY If
DISTINCT is T, eliminate duplicate rows from results

Methods specializing on different table types need not implement all
of the selection criterial.  Consult the documentation for each
method.
"))


(defgeneric value (table &key where column-name &allow-other-keys)
  (:documentation 
"Return a TABLE value.

WHERE is a function of ROW and returns T or a generalized boolean for
the desired row.

COLUMN-NAME is a column name as stored "))

