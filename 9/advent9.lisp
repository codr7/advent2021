(defstruct pt
  x y)

(defun pt (x y)
  (make-pt :x x :y y))

(defstruct ln
  beg end)

(defun ln (beg end)
  (make-ln :beg beg :end end))

(defparameter *lines* (list (ln (pt 0 9) (pt 5 9))
			    (ln (pt 8 0) (pt 0 8))
			    (ln (pt 9 4) (pt 3 4))
			    (ln (pt 2 2) (pt 2 1))
			    (ln (pt 7 0) (pt 7 4))
			    (ln (pt 6 4) (pt 2 0))
			    (ln (pt 0 9) (pt 2 9))
			    (ln (pt 3 4) (pt 1 4))
			    (ln (pt 0 0) (pt 8 8))
			    (ln (pt 5 5) (pt 8 2))))

(defun parse-pt (s)
  (let ((i (search "," s)))
    (pt (parse-integer (subseq s 0 i))
	(parse-integer (subseq s (1+ i))))))

(defun parse-ln (s)
  (let ((i (search "->" s)))
    (ln (parse-pt (subseq s 0 i)) (parse-pt (subseq s (+ i 2))))))

(defun plot-ln (ln out)
  (let ((x (pt-x (ln-beg ln)))
	(y (pt-y (ln-beg ln)))
	(end-x (pt-x (ln-end ln)))
	(end-y (pt-y (ln-end ln))))
    (labels ((rec ()	       
	       (incf (aref out y x))

	       (when (and (= x end-x) (= y end-y))
		 (return-from rec))

	       (when (< x end-x)
		 (incf x))

	       (when (> x end-x)
		 (decf x))

	       (when (< y end-y)
		 (incf y))

	       (when (> y end-y)
		 (decf y))

	       (rec)))

      (rec))))

(defun plot (lns)
  (let ((max-x 0) (max-y 0))
    (dolist (ln lns)
      (setf max-x (max max-x (pt-x (ln-beg ln)) (pt-x (ln-end ln)))
	    max-y (max max-y (pt-y (ln-beg ln)) (pt-y (ln-end ln)))))
    (let ((out (make-array `(,(1+ max-y) ,(1+ max-x)) :element-type 'integer :initial-element 0)))
      (dolist (ln lns)
	(plot-ln ln out))
      out)))

(defun straight (lns)
  (remove-if-not (lambda (l) (or (= (pt-x (ln-beg l)) (pt-x (ln-end l)))
				 (= (pt-y (ln-beg l)) (pt-y (ln-end l)))))
		 lns))

(defun risk (p)
  (let ((x-max (array-dimension p 0))
	(y-max (array-dimension p 1))
	(n 0))
    (dotimes (x x-max)
      (dotimes (y y-max)
	(when (> (aref p x y) 1)
	  (incf n))))
    n))

(defun test () 
  (format t "~a~%~a~%" (plot *lines*) (risk (plot *lines*)))

  (with-open-file (in "input")
    (labels ((read-ls (out)
	       (let ((s (read-line in nil)))
		 (if s
		     (read-ls (cons (parse-ln s) out))
		     (nreverse out)))))
      (let ((ls (read-ls nil)))
	(format t "answer: ~a ~a~%" (length ls) (risk (plot ls)))))))
