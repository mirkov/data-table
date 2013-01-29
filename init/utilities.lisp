(in-package :numeric-table)

#+skip(export '(init-vv-array vvref))

(defun mklist (thing)
  "Ensure that THING is a list.  If it is not, wrap it inside a list."
  (if (listp thing) thing (list thing)))









(defgeneric make-vector (length column-schema)
  (:documentation 
"Use LENGTH and COLUMN-SCHEMA and return a vector of correct length
and type"))

(defun add-column-schema-short+long-names (short-name long-name
					   &rest short-long-names)
  (labels ((add-short+long (short long)
	     (setf *valid-column-schema*
		   (remove short *valid-column-schema* :key #'car :test #'eq))
	     (push (cons short long) *valid-column-schema*)))
    (add-short+long short-name long-name)
    (loop
       :for sublist :on short-long-names :by #'cddr
       :do (add-short+long (car sublist) (cadr sublist)))))


(defun compare-nested-vectors (ref-nv test-nv
			       &key (test #'equal)
				 (target-rows
				  (loop for i below (nested-vectors:row-count test-nv)
				     collect i))
				 (target-columns
				  (loop for j below (nested-vectors:column-count test-nv)
				     collect j)))
  "Return true if elements in REF-NV match those in TEST-NV

The TEST-NV may have fewer number of rows and columns than TEST-NV.

TARGET-ROWS and TARGET-COLUMNS are lists of indices that are used to
select rows and colulmns of REF-NV that are compared against those
of TEST TABLE

TEST is a two argument function used for testing for equality."
  (notany #'null
   (nested-vectors:reduce-columns
    (lambda (arg1 &optional arg2)
      (if arg2
	  (and arg1 arg2)
	  arg1))
    (map-nested-vectors test
			(nested-vectors:select ref-nv
					       :target-rows target-rows
					       :target-columns target-columns)
			test-nv))))
  
(define-test compare-nvs
  (let ((nv1 (make-nested-vector
	      '(3 3)
	      :initial-contents '((a b c) #(1 2 3) #m(1.0 2.0 3.0))))
	(nv2 (make-nested-vector
	      '(3 3)
	      :initial-contents '((a b c) #(1 2 3) #m(1.0 2.0 3.0))))
	(nv3 (make-nested-vector
	      '(3 2)
	      :initial-contents '((a b c) #m(1.0 2.0 3.0))))
	(nv4 (make-nested-vector
	      '(2 3)
	      :initial-contents '((a  c) #(1  3) #m(1.0  3.0)))))
    (assert-true (compare-nested-vectors nv1 nv2))
    (assert-true (compare-nested-vectors nv1 nv3 :target-columns '(0 2)))
    (assert-true (compare-nested-vectors nv1 nv4 :target-rows '(0 2)))))
