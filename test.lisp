(in-package :numeric-table)

(defparameter *flower-data*
  #2a((4.9 3.0 1.4 0.2 SETOSA)
      (4.7 3.2 1.3 0.2 SETOSA)
      (4.6 3.1 1.5 0.2 SETOSA)
      (5.0 3.6 1.4 0.2 SETOSA)
      (5.4 3.9 1.7 0.4 SETOSA)
      (4.6 3.4 1.4 0.3 SETOSA)
      (5.0 3.4 1.5 0.2 SETOSA)
      (4.4 2.9 1.4 0.2 SETOSA)
      (4.9 3.1 1.5 0.1 SETOSA)
      (5.4 3.7 1.5 0.2 SETOSA))
  "Plant data copied from ...")

(defparameter *plant-table*
  (let ((table
	 (make-table 'column-major-table
		     (make-schema 'column-major-table
				  '((sepal-length number) (sepal-width number)
				    (petal-length number) (petal-width number)
				    (species symbol)))))
	(rows (array-dimension *flower-data* 0))
	(columns (array-dimension *flower-data* 1)))
    (dotimes (irow rows)
      (let ((row-values
	     (loop for icol from 0 to (1- columns) 
		collect (aref *flower-data* irow icol))))
	(insert-row row-values table)))
    table)
  "COLUMN-MAJOR-TABLE that holds values of *FLOWER-DATA*")


(define-test plant-table-column-names
  "Return column names"
  (assert-equal '(sepal-length sepal-width
		  petal-length petal-width
		  species)
		(column-names *plant-table*)))

(define-test plant-table-row-values
  "Return fifth row of the table"
  (assert-true
   (block loop-over-row-values
     (let ((table-row (nth-row 4 *plant-table*))
	   (expected-values #(5.4 3.9 1.7 0.4 SETOSA)))
       (loop for icol from 0 below (column-count *plant-table*)
	  do (print icol)
	  unless (equal (print (aref table-row icol))
			(print (aref expected-values icol)))
	  do (return-from loop-over-row-values nil))
       t))))
  
(define-test plant-table-column-values-by-name
  "Column values by name"
  (assert-numerical-equal #(1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 1.5)
			  (table-column 'petal-length *plant-table*)))

(define-test plant-table-row-values-by-index
  "Return column by index"
  (assert-numerical-equal #(1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 1.5)
			  (table-column 2 *plant-table*)))

(define-test plant-table-column-matcher
  "Check that column matcher for sepal-length and value 5.4 corresponds to fifth row"
  (assert-true
   (let* ((schema (schema *plant-table*))
	  (column (find-column 'sepal-length schema))
	  (fun (column-matcher column 5.4 *plant-table*)))
     (funcall fun 4))))



(define-test plant-table-value-get
  "Return sepal-width for the plant whose sepal-length is 5.4"
  (assert-equal
   (value *plant-table* :where (matching-rows *plant-table* '(sepal-length 5.4))
       :column-name 'sepal-width)
   3.9))
