(in-package :numeric-table)

(defun mklist (thing)
  (if (listp thing) thing (list thing)))

(defun vvref (nested-vector i-row i-col)
  (aref (aref nested-vector i-col) i-row))


(sort
