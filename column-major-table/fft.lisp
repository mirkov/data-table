(in-package :numeric-table)

(export '(fft-direction sample-delta fft-stride fft-radix-factors
	  fft-wavetable fft-workspace
	  column-dft
	  init-fft do-fft
	  float-fft
	  shifted-dft freq-axis power-spectrum))

(defclass fft ()
  ((direction :initform nil
	      :initarg :fft-direction
	      :accessor fft-direction
	      :documentation "Holds the direction of the computed FFT.

Can be either :forward :inverse or :backward

:inverse and :forward are reverse of each other.  :backward same as
inverse, except for the missing 1/N division")
   (delta :initform 1d0
	  :initarg :delta
	  :accessor sample-delta
	  :documentation "Sampling length")
   (stride :initform 1
	   :initarg :stride
	   :accessor stride
	   :documentation "Stride for doing FFT's of matrices packed as vectors")
   (dft :initform nil
	:accessor dft
	:documentation "Holds the calculated DFT in an uncompressed form.

That means that the DFT of a real or imaginary sequence of length N is
stored as a complex vector of length N")
   (power-of-2? :accessor power-of-2?
		:documentation "T or nil depending whether the data-length is a power of 2")
   (fft-len :documentation "Holds the length of the calculated FFT")
   (mixed-radix-factors :accessor fft-radix-factors
    :documentation "Holds the factors for a mix-radix FFT")
   (wavetable :initarg :fft-wavetable
	      :accessor fft-wavetable
	      :documentation "Either the wavetable, or the column name
or schema that holds the wavetable.

The schema could belong to another table, as long as the dimensions
are correct.")
   (workspace :initarg :fft-workspace
	      :accessor fft-workspace
	      :documentation "Either the workspace, or the column name
or schema that holds the workspace.

The schema could belong to another table, as long as the dimensions
are correct."))
  (:documentation "Specification of an FFT transform

This is a virtual class, meant to be inherited by actual classes"))



(defclass float-fft-column-schema (foreign-double-schema fft)
  ()
  (:documentation "FFT of real data"))


(add-column-schema-short+long-names 'float-fft 'float-fft-column-schema)

#+foreign-complex-implemented
(defclass complex-fft (foreign-complex-schema fft)
  ()
  (:documentation "FFT of real data"))

(defgeneric do-fft (table column)
  (:documentation "Perform an FFT for a TABLE's COLUMN and store the DFT. 

The DFT is stored in unpacked and unshifted format.  Thus for a DFT of
a real valued vector, of length N, we store a complex vector of length
N.")
  (:method ((table column-major-table) (column-name symbol))
    (do-fft table (find-column-schema column-name table))))

(defgeneric init-fft (table column)
  (:documentation "Setup the variables for the fft")
  (:method ((table column-major-table) (column-name symbol))
    (init-fft table (find-column-schema column-name table)))
  (:method ((table column-major-table) (column-schema float-fft-column-schema))
    (setf (fft-wavetable column-schema)
	  (gsll:make-fft-wavetable 'double-float
				   (row-count table))
	  (fft-workspace column-schema)
	  (gsll:make-fft-workspace 'double-float
				   (row-count table))
	  (power-of-2? column-schema) (= 1 (logcount (row-count table))))))

(defmethod do-fft ((table column-major-table)
		   (column-schema float-fft-column-schema))
  (let ((stride (stride column-schema))
	(i-column (i-column column-schema)))
    (setf (dft column-schema)
	  (gsll:unpack
	   (if (power-of-2? column-schema)
	       (gsll::forward-fourier-transform-radix2
		(grid:copy (nth-column i-column table))
		:stride stride)
	       (gsll::forward-fourier-transform-nonradix2
		(grid:copy (nth-column (i-column column-schema) table))
		:stride stride
		:workspace (fft-workspace column-schema)
		:wavetable (fft-wavetable column-schema)))
	   :stride stride :unpack-type 'complex))
    (values)))



(defun fft-128 ()
  (let ((table (make-table 'column-major-table
			   (make-table-schema
			    'column-major-table
			    '((data float-fft))))))
    (setf (table-column 'data table)
	  (gsll::fft-pulse-test 'double-float 128))
    (init-fft table 'data)
    (do-fft table 'data)
    (column-dft 'data table)
    table))

(defun fft-630 ()
  (let ((table (make-table 'column-major-table
			   (make-table-schema
			    'column-major-table
			    '((data float-fft))))))
    (setf (table-column 'data table)
	  (gsll::fft-pulse-test 'double-float 630))
    (init-fft table 'data)
    (do-fft table 'data)
    (column-dft 'data table)
    table))

(defgeneric column-dft (column-name table &key shifted freq-axis)
  (:documentation "Return the stored DFT for column in table

COLUMN-NAME is the column name, a symbol
TABLE is a column-major-table

If keyword SHIFTED is T, return the shifted DTF")
  (:method ((column-name symbol) (table column-major-table)
	    &key shifted freq-axis)
    (let* ((column-schema (find-column-schema column-name table))
	   (dft (dft column-schema))
	   (axis (when freq-axis
		   (gsll:fft-frequency-vector 'double-float (row-count table)
					      :sample-spacing
					      (sample-delta column-schema)
					      :shifted shifted))))
      (values
       (if shifted
	   (gsll:fft-shift dft :stride (stride column-schema))
	   dft)
       axis))))

(defgeneric power-spectrum (column-schema table &key shifted freq-axis)
  (:documentation "Return the power spectrum from the DFT in COLUMN-SCHEMA

COLUMN-SCHEMA is either the schema or its name")
  (:method ((column-name symbol) (table column-major-table)
	    &key shifted freq-axis)
    (power-spectrum (find-column-schema column-name table) table
		    :shifted shifted :freq-axis freq-axis))
  (:method ((column-schema fft) (table column-major-table)
	    &key shifted freq-axis)
    (let* ((N (row-count table))
	   (ps (grid:make-foreign-array
		'double-float
		:dimensions N))
	   (dft (dft column-schema))
	   (axis (when freq-axis
		   (gsll:fft-frequency-vector 'double-float (row-count table)
					      :sample-spacing
					      (sample-delta column-schema)
					      :shifted shifted))))
      (dotimes (i N)
	(setf (grid:aref ps i)
	      (let ((dft-i (grid:aref dft i)))
		(realpart (* dft-i 
			     (conjugate dft-i))))))
      (values
       (if shifted
	   (gsll:fft-shift ps :stride (stride column-schema))
	   ps)
       axis))))
