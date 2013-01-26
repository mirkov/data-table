(in-package :numeric-table)

;; test waveforms
(defun sin-train (n delta phase)
  "Phase is in radians"
  (coerce
   (loop for i below n
      collect (sin (+ (* i delta) phase)))
   'array))

(defun square-pulse (n delta phase width/2)
  (let ((symmetric-pulse
	 (coerce
	  (loop for i below n
	     collect (let* ((time (* i delta)))
		       (if (or (< time width/2)
			       (> time (- n width/2)))
			   1.0
			   0.0)))
	  'array))
	(pulse (make-array n)))
    (loop for i below n
       do (setf (aref pulse i)
		(aref symmetric-pulse (mod (+ i phase) n))))
    pulse))


(defparameter *pulses*
  (let ((table
	 (make-table 'column-major-table
		     (make-table-schema 'column-major-table
					'((time number)
					  (sine float-fft)
					  (pulse float-fft))))))
    (let ((time (loop for i below 240
		     collect i)))
      (setf (table-column 'time table) (coerce time 'vector))
      (setf (table-column 'sine table)
	    (sin-train (row-count table) 1.0 0.0)
	    (table-column 'pulse table)
	    (square-pulse (row-count table) 1 0 10))
      (init-fft table 'sine)
      (init-fft table 'pulse)
      #+skip(setf (fft-wavetable (table-column 'pulse table))
	    (fft-wavetable (table-column 'sine table))
	    (fft-workspace (table-column 'pulse table))
	    (fft-workspace (table-column 'sine table)))
      table)))

(progn
  (do-fft *pulses* 'sine)
  (do-fft *pulses* 'pulse))
