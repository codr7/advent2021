(defun depth-inc (&rest ms)
  (labels ((rec (in prev out)
	     (if in
		 (let ((m (pop in)))
		   (when (and prev (> m prev))
		     (incf out))
		   (rec in m out))
		 out)))
    (rec ms nil 0)))

(defun test ()
  (assert (= (depth-inc 199 200 208 210 200 207 240 269 260 263)
	     7))

  (with-open-file (in "input")
    (labels ((read-ms (out)
	       (let ((m (read-line in nil)))
		 (if m
		     (read-ms (cons (parse-integer m) out))
		     (nreverse out)))))
      (let ((ms (read-ms nil)))
	(format t "answer: ~a ~a~%" (length ms) (apply #'depth-inc ms))))))
