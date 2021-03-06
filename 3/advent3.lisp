(defun get-pos (&rest cs)
  (let ((hpos 0) (depth 0))
    (dolist (c cs)
      (let ((id (first c))
	    (v (rest c)))
	(ecase id
	  (down (incf depth v))
	  (forward (incf hpos v))
	  (up (decf depth v)))))
    (* hpos depth)))

(defun test ()
  (assert (= (apply #'get-pos '((forward . 5)
				(down . 5)
				(forward . 8)
				(up . 3)
				(down . 8)
				(forward . 2)))
	     150))
  
  (with-open-file (in "input")
    (labels ((read-cs (out)
	       (if in
		   (let ((id (read in nil)))
		     (if id
			 (read-cs (cons (cons id (read in)) out))
			 (nreverse out))))))
      (let ((cs (read-cs nil)))
	(format t "answer: ~a ~a~%" (length cs) (apply #'get-pos cs))))))
