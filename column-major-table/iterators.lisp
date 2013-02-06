(in-package :iterate)

(defclause-sequence in-nt-row row-index-of-nt
  :access-fn
  '(lambda (column-major-table index)
    (numeric-table:nth-row column-major-table index))
  :size-fn
  '(lambda (column-major-table)
    (numeric-table:row-count column-major-table))
  :element-type 'column-major-table-row
  :sequence-type 'column-major-table
  :element-doc-string "Accessor to a column-major-table row"
  :index-doc-string "Row index")
