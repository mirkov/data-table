(in-package :numeric-table)


(export '(numeric-table
	  row-count column-count
	  data-source data-author table-description
	  make-table table-column set-table-column
	  nth-column set-nth-column
	  insert-row nth-row
	  table-size init-storage
	  ;;
	  make-table-schema
	  table-schema
	  make-column-schema
	  value-normalizer
	  ;;
	  object
	  ;;
	  column-names column-name column-doc empty-value
	  find-column-schema nth-column-schema
	  ;;
	  select value in
	  ))

(defclass numeric-table ()
  ((table-data :initarg :table-data
	  :reader table-data
	  :documentation "Table data")
   (row-count :accessor row-count
	      :initform 0
	      :documentation "Return number of rows
In case of variable row count return values 'variable and max-row-count")
   (column-count :reader column-count
		 :documentation "Return number of columns
In case of a variable column count, return values 'variable and max-column-count"
		 :initform 0)
   (table-schema :accessor table-schema
		 :initarg :table-schema
		 :documentation "Stores table meta-data 

- Column names
- Functions for setting and comparing column values")
   (build-method :reader build-method
		 :initarg :build-method
		 :initform nil
		 :documentation "A symbol specifying the build method")
   (description :accessor table-description
		:initform ""
		:documentation "Use to store the description of the data")
   (data-source :accessor data-source
		:initarg :data-source
		:documentation "The physical origin of the data

It can be a file, a computation, interactive, or something else")
   (data-author :accessor data-author
		:initarg :data-author
		:documentation "Who were the people that created the data

This can be a journal reference or something else"))
  (:documentation "Stores a numeric table"))


(defmethod initialize-instance :after ((self numeric-table)
				 &key table-schema build-method
				   data-source data-author
				   table-data)
  (setf (slot-value self 'table-schema) table-schema)
  (awhen build-method
    (setf (slot-value self 'build-method) it))
  (awhen data-source
    (setf (data-source self) it))
  (awhen data-author
    (setf (data-author self) it))
  (awhen table-data
    (setf (slot-value self 'table-data) it)))



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


#+skip(defgeneric set-table-column (column-name table column-vector
				    &key overwrite
				    &allow-other-keys)
  (:documentation 
"Load COLUMN-VECTOR into TABLE column specified by COLUMN-NAME

OVERWRITE, if T, allows an existing column to be overwritten"))
#+skip(defsetf table-column set-table-column
    "Set table column using SET-TABLE-COLUMN")

(defgeneric nth-column (n table)
  (:documentation "Return the N-th column of TABLE"))

#+skip (defgeneric set-nth-column (n table column-vector
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
  ((name :reader column-name
	 :initarg :name)
   ;; following slots are specialized by more specific classes
   (equality-predicate :reader equality-predicate
		       :initform nil)
   (comparator :reader comparator
	       :initform nil)
   (default-type :reader default-type
     :initarg :default-type
     :initform nil
     :documentation "Stored values will be compared against the type")
   ;; The remaining slots are specified when the column schema is instantiated
   (value-normalizer :reader value-normalizer
		     :initarg :value-normalizer
		     :initform #'(lambda (v column)
				   (declare (ignore column))
				   v))
   (default-value :reader default-value
     :initarg :default-value
     :initform nil)
   (i-column :reader i-column
	     :initarg :i-column
	     :initform nil
	     :documentation "Stores the column index")
   (documentation :initarg :documentation
		  :reader column-doc
		  :initform nil
		  :documentation "Text describing the documentation")
   (empty-value :initarg :empty-value
		:initform nil
		:reader empty-value
		:documentation "Value that signifies an empty cell"))
  (:documentation "Basic column schema

This is a virtual class, not meant to be instantiated.  All of its
slots are uninitialized.  They will be initialized by the
sub-classes"))

(defmethod describe-object ((self column-schema) stream)
  (format stream "Column schema ~a:~%" (column-name self))
  (format stream "It is stored in column ~a~%" (i-column self)))

(defclass string-column-schema (column-schema)
  ((equality-predicate :reader equality-predicate
		       :initform #'string=)
   (comparator :initform #'string<)
   (default-type :initform 'string))
  (:documentation "Column schema for storing string values"))

(defclass number-column-schema (column-schema)
  ((equality-predicate :reader equality-predicate
		       :initform #'=)
   (comparator :initform #'<)
   (default-type :initform 'number))
  (:documentation "Column schema for storing numerical values"))

(defclass symbol-column-schema (column-schema)
  ((equality-predicate :reader equality-predicate
		       :initform #'eq)
   (comparator :initform #'string<)
   (default-type :initform 'symbol))
  (:documentation "Column schema for storing symbols"))

(defclass object-column-schema (column-schema)
  ((equality-predicate :initform #'eq
		       :documentation
"By default objects are equal if they are the same object, not only if
they are of the same class")
   (comparator :initform (lambda (arg1 arg2)
			   (declare (ignore arg1 arg2))
			   (error "Camparator not defined for `object-column-schema'")))
   (default-type :initform 'class))
  (:documentation "Column schema for storing objects"))

(defclass function-column-schema (column-schema)
  ((equality-predicate :initform #'eq
		       :documentation
"Two functions are equal if they are same (symbol-function)")
   (comparator :initform (lambda (arg1 arg2)
			   (declare (ignore arg1 arg2))
			   (error "Camparator not defined for `function-column-schema'")))
   (default-type :initform 'function))
  (:documentation "Column schema for storing functions"))


;; I am not sure that value-normalization should check for type
;; correctness
#+skip
(defmethod initialize-instance :after ((self (eql 'column-schema)) &key)
  (with-slots (value-normalizer default-type) self
    (setf value-normalizer
	  #'(lambda (value column-schema)
	      (declare (ignore column-schema))
	      (assert (typep value default-type))))))

;;; Code dealing with short and long names for column-schema
(defparameter *valid-column-schema*
  nil
  "A list of valid column schemas.  It stores a short and a long form.
  The short form, is used by users, and the long-form, that refers to
  the class name")


(add-column-schema-short+long-names 'string 'string-column-schema
				    'number 'number-column-schema
				    'symbol 'symbol-column-schema
				    'object 'object-column-schema
				    'function 'function-column-schema)

(defun column-schema-long-name (short-name)
  "Return long-name for column-schema"
  (let ((long-name (cdr (assoc short-name *valid-column-schema*))))
    (assert long-name ()
	    "Short name: ~a, is not valid" short-name)
    long-name))

;;; End of code dealing with long and short names for column-schema

(defun make-column-schema (name type
				&key documentation empty-value
				value-normalizer)
  "Return instance of a column-schema "
  (let ((column-schema 
	 (make-instance (column-schema-long-name type)
			:name name
			:documentation documentation
			:empty-value empty-value)))
    (awhen value-normalizer
      (setf (slot-value column-schema 'value-normalizer) it))
    column-schema))


(defgeneric normalize-value (value schema)
  (:documentation "Normalize VALUE according to SCHEMA

SCHEMA can be a COLUMN-SCHEMA or (when implemented) ROW-SCHEMA")
  (:method (value (column-schema column-schema))
    (funcall (value-normalizer column-schema)
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
    (column-doc (find-column-schema column-name table-schema)))
  (:method ((column-name symbol) (table numeric-table))
    (column-doc (find-column-schema column-name 
				    (table-schema table)))))


(defgeneric init-storage (table)
  (:documentation "Initialize table's storage"))


(defgeneric find-column-schema (column-name table-or-table-schema)
  (:documentation "Return COLUMN-SCHEMA by COLUMN-NAME from either a
  TABLE or its TABLE-SCHEMA

NAME is a symbol by which the column is identified in the TABLE-SCHEMA
CONTAINER can be either a table, or a table-schema")
  (:method ((name symbol) (table numeric-table))
    (find name (table-schema table) :key #'column-name))
  (:method ((name symbol) (table-schema cons))
    (find name table-schema :key #'column-name)))

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
       collect (find-column-schema c table-schema))))

(defgeneric restrict-rows (table where column-schema)
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

(defgeneric read-table (stream table)
  (:documentation "Read TABLE from STREAM"))
