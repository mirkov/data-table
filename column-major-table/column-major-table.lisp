;; Mirko Vukovic
;; Time-stamp: <2013-01-03 11:58:18Eastern Standard Time column-major-table.lisp>
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

(define-test make-table-schema
  (let ((schema
	 (make-table-schema 'column-major-table '((delta number) (lambda-pp number)))))
    (assert-number-equal 0 (slot-value (first schema) 'i-column))
    (assert-number-equal 1 (slot-value (second schema) 'i-column))
    (assert-equal 'delta (slot-value (first schema) 'name))))


(defmethod column-documentation ((column-name symbol)
				  (table column-major-table))
  (column-documentation column-name (table-schema table)))

(defmethod make-table ((type (eql 'column-major-table)) schema
		       &key build-method data-source data-author)
  (let ((table (make-instance type
			      :table-schema schema)))
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
			   (make-table-schema 'column-major-table
					'((delta number) (lambda-pp number))))))


;;; methods for row insertion





(defmacro with-bare-test-table (&body body)
  `(let ((test-table
	 (make-table 'column-major-table
		     (make-table-schema 'column-major-table
				  '((delta number) (lambda-pp number))))))
     ,@body))



(defparameter *test-table*
  (with-bare-test-table
    (insert-row '(3 4) test-table)
    (insert-row '(13 14) test-table)
    test-table))

(define-test *test-table*
  (first (table-schema *test-table*)))


;;; Selection and value return functions modeled after PCL 392






  
