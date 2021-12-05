(defun depth-inc1 (&rest ms)
  (labels ((rec (in prev out)
	     (if in
		 (let ((m (pop in)))
		   (when (and prev (> m prev))
		     (incf out))
		   (rec in m out))
		 out)))
    (rec ms nil 0)))

(defun depth-inc2 (&rest ms)
  (labels ((rec (in out)
	     (if (> (length in) 2)
		 (let ((m1 (pop in))
		       (m2 (first in))
		       (m3 (second in)))
		   (rec in (cons (+ m1 m2 m3) out)))
		 (nreverse out))))
    (apply #'depth-inc1 (rec ms nil))))

(defun test ()
  (assert (= (depth-inc2 199 200 208 210 200 207 240 269 260 263) 5))

  (with-open-file (in "~/Projects/advent/1/input")
    (labels ((read-ms (out)
	       (let ((m (read-line in nil)))
		 (if m
		     (read-ms (cons (parse-integer m) out))
		     (nreverse out)))))
      (let ((ms (read-ms nil)))
	(format t "answer: ~a ~a~%" (length ms) (apply #'depth-inc2 ms))))))