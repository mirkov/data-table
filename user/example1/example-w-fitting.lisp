(in-package :ntu)

"In this file, I try fitting the data using linear and non-linear
fitting.  I will obtain the data from the raw-data, with missing
values removed.

I create two data sub-sets using `select'.  One in the range
1650-1850, and the other 1920 to the end.  I will first do a linear
fit on the log of the two data sets.  

Fitting methods are defined using the column schema type.  The column
schema also stores the fitting results.

For each data-set I need its own column schema, even though they are
identical.  This is a consequence of how `select' works: when it
generates a new table, it re-uses the source table's schema.  Thus, if
I use select twice on the same table, the two tables will share the
column schemas, and thus the fitting results.

In the future I hope to modify `select' to optionaly generate a new
schema for the new table.

For non-linear fits, I need to provide the fit function and the
Jacobian.  I do not need to provide the residual's function, or the
residuals+Jacobian combo function.  These are generated on the fly
during the initialization of the non-linear fit.
"


(defun table-schema-log-lin-fit ()
  "Return column schema for an ax+b least squares fit of the log of
the population data"
  (make-table-schema 'numeric-table:column-major-table
		     (list `(year foreign-double :documentation "Year")
			   `(population ax+b-ls-fit :documentation "Population"
			     :value-normalizer ,(lambda (value column-schema)
							(float (log value 10) 1d0))))))

(defparameter *1650-1850-log-lin-fit*
  (let ((table
	 (make-table
	  'column-major-table (table-schema-log-lin-fit))))
    (setf (table-column 'population table)
	  (table-column 'population *table-non-empty-rows*)
	  (table-column 'year table)
	  (table-column 'year *table-non-empty-rows*))
    (setf (independent-var table 'population) 'year)
    (setf table
	  (select table :where (matching-rows table
					`(year 1650 >=)
					`(year 1850 <=))))
    (fit-column table 'population)
    table)
  "Log-scaled data between 1650 and 1850, with a linear fit

Here, we defined the table, added the data, and then pruned for the
desired year range.  Finally we called the fitting procedure")

;; to evaluate the fit, we use the `evaluate' function, common to both
;; fitting and interpolation.  Remember, the fit is on log10 of the
;; data, and so is evaluate.  No reverse un-normalization is done.
(evaluate *1650-1850-log-lin-fit* 'population 1650)

(defparameter *1920-2010-log-lin-fit*
  (let ((table
	 (make-table
	  'column-major-table (table-schema-log-lin-fit))))
    (setf (table-column 'population table)
	  (table-column 'population *table-non-empty-rows*)
	  (table-column 'year table)
	  (table-column 'year *table-non-empty-rows*))
    (setf (independent-var table 'population) 'year)
    (setf table
	  (select table :where (matching-rows table
					      `(year 1920 >=))))
    (fit-column table 'population)
    table)
  "Log-scaled data between 1920-2010, with a linear fit

Here, we defined the table, added the data, and then pruned for the
desired year range.  Finally we called the fitting procedure")
  
(evaluate *1920-2010-log-lin-fit* 'population 2010)

;; we can obtain fit coefficients.  This function currently exposes
;; too much of the code internals.  The interface will change.
(fit-coeffs (find-column-schema 'population *1650-1850-LOG-LIN-FIT*))

;;; We now plot the two fits, and the raw data, interpolated using
;;; akima's fitPlot of the two fits and the akima fit
#|
(gnuplot:with-png-output ("linear-fits.png" *png-dir*)
 ;; evaluating the fits for all years	;
 (destructuring-bind (1650-1850-fit 1920-2010-fit)
     (mapcar
      (lambda (table)
	(coerce
	 (loop for year across *years*
	    collect (evaluate table 'population (float year 1d0)))
	 'array))
      `(,*1650-1850-LOG-LIN-FIT* ,*1920-2010-LOG-LIN-FIT*))
  ;; evaluating the akima interpolation	;
   (let ((a-interpol
	  (coerce
	   (loop for year across *years*
	      collect (evaluate *interpolation-table* 'population-a (float year 1d0)))
	   'array)))
  ;; plot part				; ;
     (gnuplot:set-to ((yrange '(3 10))
		      (xlabel "Year")
		      (ylabel "Log_{10} Population")
		      #+skip(title "Comparison of polynomial, spline, and Akima"))
       (gnuplot:plot-xys *years*
			 `((,1650-1850-fit :title "1650-1850 fit")
			   (,1920-2010-fit :title "1920-2010 fit")
			   (,a-interpol :title "Akima interpolation")))))))
|#

;;; we next do a non-linear fit on the 1650-1850 range
(defun a*10^/bx/ (x coeffs)
  "Function a 10^(-bx)

Coeffs is a two element grid: #m(a b)

Because the y-data covers a very wide range, I normalized the fit
function to the data in year 1650.  Then the guess for coefficient `a'
is simple: it is the population in year 1650. 
"
  (let ((a (grid:aref coeffs 0))
	(b (grid:aref coeffs 1))
	(x-rel (- x 1650.0)))
    (* a (expt 10.0 (* b x-rel)))))

(defun a*10^/bx/-jacobian (x coeffs)
  "Jacobian of y with respect to a and b"
  (let* ((a (grid:aref coeffs 0))
	 (b (grid:aref coeffs 1))
	 (x-rel (- x 1650.0))
	 (e (expt 10.0 (* b x-rel))))
    (mapcar (lambda (arg)
	      (* 1 arg))
	    (list e
		  (* (log 10.0) a x-rel e)))))

(defparameter *1650-1850-non-lin-fit*
  (let ((table
	 (make-table
	  'column-major-table 
	  (make-table-schema
	   'numeric-table:column-major-table
	   (list `(year foreign-double :documentation "Year")
		 `(population nonlinear-ls-sq-column
			      :documentation "Population"
			      :value-normalizer
			      ,(lambda (value column-schema)
				       (declare (ignore column-schema))
				       (float value 1d0))))))))
    (setf (table-column 'population table)
	  (table-column 'population *table-non-empty-rows*)
	  (table-column 'year table)
	  (table-column 'year *table-non-empty-rows*))
    (setf (independent-var table 'population) 'year
	  ;;(sigma (find-column-schema 'population table)) 'population
	  )
    (setf table
	  (select table :where (matching-rows table
					`(year 1650 >=)
					`(year 1850 <=))))
    (init-nonlin-column-fit 'population table
			    #'a*10^/bx/ #'a*10^/bx/-jacobian 2)
    table)
  "Data between 1650 and 1850, initialized for non-linear fitting

Here, we defined the table, added the data, and then prune it for the
desired year range.  Finally we initialize the fitting procedure

Weighing has not been exhaustively tested.")


;;; Next we perform the non-linear fit.  The coefficients are stored in the schema
(let* ((lin-fit-coeffs (fit-coeffs (find-column-schema 'population *1650-1850-log-lin-fit*)))
       (B-guess (second lin-fit-coeffs)))
  (let ((coeffs (grid:make-foreign-array
		 'double-float
		 :dimensions 2
		 :initial-contents
		 (list (float
			(value *table-non-empty-rows*
			       :column-name 'population
			       :where (matching-rows
				       *table-non-empty-rows*
				       `(year 1650)))
			1d0)
		       B-guess))))
    (fit-column *1650-1850-NON-LIN-FIT* 'population
		:coeffs-guess coeffs
		:print-steps t)))


;; we can evaluate the fit.  The fit function was stored in the column
;; schema.  Warning: if we recompile the function, the column schema
;; will retain the pointer to the old function.
(evaluate *1650-1850-non-lin-fit* 'population 1650.0)

;;; Plot of the two fits and the akima fit
#|
(gnuplot:with-png-output ("non-linear-fits.png" *png-dir*)
 ;; evaluating the fits for all years	;
  (let ((linear-fit (coerce
		     (loop for year across *years*
			collect (expt 10.0
				      (evaluate *1650-1850-log-lin-fit*
						'population (float year 1d0))))
		     'array))
	(nonlinear-fit (coerce
			(loop for year across *years*
			   collect (evaluate *1650-1850-non-lin-fit*
					     'population (float year 1d0)))
			'array))
	(a-interpol
	 (coerce
	  (loop for year across *years*
	     collect (expt 10.0
			   (evaluate *interpolation-table*
				     'population-a (float year 1d0))))
	  'array)))
    ;; plot part				; ;
    (gnuplot:set-to ((xlabel "Year")
		     (ylabel "Population")
		     (logscale :y)
		     (title "Comparison of log-linear, non-linear fits"))
      (gnuplot:plot-xys *years*
			`((,linear-fit :title "linear")
			  (,nonlinear-fit :title "nonlinear")
			  (,a-interpol :title "a-interpol"))))))
|#
