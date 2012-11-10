(in-package :numeric-table)

(defun mklist (thing)
  (if (listp thing) thing (list thing)))

(defun make-vv-array (columns)
  "Return a nested vector array

The outer vector is of length COLUMNS

Each of its elements is initialized as an adjustable vector of
dimension 0 and fill-pointer set to 0"
  (let ((vv-array (make-array columns)))
    (dotimes (j columns)
      (setf (aref vv-array j) (make-array 0 :adjustable t :fill-pointer 0)))
    vv-array))


(defun vvref (nested-vector i-row i-col)
  (aref (aref nested-vector i-col) i-row))



