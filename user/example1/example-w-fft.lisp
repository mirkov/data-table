(in-package :ntu)

"Calculation of a Discrete Fourier Transform using the Fast Fourier
Transform algorithm of GSLL.

We calculate the power spectrum of the annual solar spot number.  The
data is obtained from the NOAA web site:
ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SUNSPOT_NUMBERS/GROUP_SUNSPOT_NUMBERS/yearrg.dat

The float-fft column schema is set-up to calculate the DFT of a data
of real numbers.  GSL(L) had capability for handling strides in the
data, but I have not tested that - I use a stride of 1.

"

#|
(multiple-value-bind (ps axis)
    (power-spectrum 'sine *pulses* :shifted t :freq-axis t)
  (gnuplot:plot-xy axis ps))

|#


(defparameter *solar-spots-file*
  (let* ((rel-file-name "data-files/yearrg.dat")
	 (pathname
	   (probe-file
	    (merge-pathnames rel-file-name
			     (asdf:system-source-directory "numeric-table-user")))))
    (assert pathname ()
	    "Datafile: ~a not found" rel-file-name)
    pathname)
  "Pathname to file solar spots data")

(defparameter *solar-spots*
  (let ((raw-data (make-table 'column-major-table
			      (make-table-schema 'column-major-table
						 '((year number)
						   (spots foreign-double))))))
    (with-input-from-file (stream *solar-spots-file*)
      (read-line stream)
      (read-table stream raw-data))
    (let* ((yearly-average (gsll:mean (table-column 'spots raw-data)))
	   (n (row-count raw-data))
	   (table (make-table 'column-major-table
			      (make-table-schema 'column-major-table
						 '((year foreign-double)
						   (spots float-fft)
						   (spots~ float-fft))))))
      (setf (table-column 'year table)
	    (table-column 'year raw-data)
	    (table-column 'spots table)
	    (table-column 'spots raw-data)
	    (table-column 'spots~ table)
	    (let ((spots~ (grid:make-foreign-array 'double-float
						   :dimensions `(,n)))
		  (spots (table-column 'spots table)))
	      (dotimes (i n)
		(setf (grid:aref spots~ i)
		      (- (grid:aref spots i) yearly-average)))
	      spots~))
      (init-fft table 'spots)
      (do-fft table 'spots)
      (init-fft table 'spots~)
      (do-fft table 'spots~)
      table))
  "Table with of annual solar spot number, along with the DFT of the data.

After loading the data, we do `init-fft'.  This sets up the workspaces
and wavetables for dealing with data that is not of length 2^n.  

`do-fft' calculates the fft.  Since the data if of float type, GSL
packs the FFT to a real array of length N.  We use the unpack routines
to unpack the data into a complex array of length N.

The code currently does not allow wavetable and workspace sharing
which should be possible for data of the same length.


The power spectrum shows a peak at frequences 0 and 0.9.  The first is
corresponds to the yearly mean, and the second to the 11-year cycle.

There also seems to be a very weak peak at 0.45/year (2.2 years).
This could concievably be resolved better with monthly data.

Plot of the raw data suggests a variabily on the 100 year scale.  To
test that, I generated another data set, with the mean removed. The
spectrum peaks around 0.005/year or about 200 year period.
Considering that the data covers only 400 years, it would be a stretch
to claim anything like that.
")


#|


(gnuplot:with-png-output ("solar-sunsport-by-year.png" *png-dir*)
  (gnuplot:set-to
      ((title "Yearly sunspot number")
       (xlabel "Year"))
    (gnuplot:plot-xy (table-column 'year *solar-spots*)
		     (table-column 'spots *solar-spots*))))

(gnuplot:with-png-output ("solar-sunsport-maunder-minimum.png" *png-dir*)
  (gnuplot:set-to
      ((title "Yearly sunspot number")
       (xlabel "Year")
       (xrange '(1650 1710)))
    (gnuplot:plot-xy (table-column 'year *solar-spots*)
		     (table-column 'spots *solar-spots*))))

(gnuplot:with-png-output ("solar-sunsport-power-spectrum.png" *png-dir*)
  (gnuplot:set-to
      ((title "Power spectrum of yearly sunspot number")
       (logscale :Y)
       (xlabel "1/year")
       (yrange '(1e4 *)))
    (multiple-value-bind (ps freq)
	(power-spectrum 'spots *solar-spots* :shifted t
			:freq-axis t)
      (gnuplot:plot-xy freq
		       ps))))


(gnuplot:with-png-output ("solar-sunsport-power-spectrum-zoom.png" *png-dir*)
  (gnuplot:set-to
      ((title "Power spectrum of yearly sunspot number")
       (logscale :Y)
       (xlabel "1/year")
       (xrange '(-0.11 0.11))
       (yrange '(1e4 *)))
    (multiple-value-bind (ps freq)
	(power-spectrum 'spots *solar-spots* :shifted t
			:freq-axis t)
      (let ((ps~ (power-spectrum 'spots~ *solar-spots* :shifted t)))
      (gnuplot:plot-xys freq
			`((,ps :title "raw data spectrum")
			  (,ps~ :title "mean-removed spectrum")))))))

|#
