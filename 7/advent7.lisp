(defun make-board (&rest args)
  (apply #'make-array 25 args))

(defun board (&rest numbers)
  (make-board :initial-contents numbers))

(defparameter *boards* (list (board 22 13 17 11  0
				     8  2 23  4 24
				    21  9 14 16  7
				     6 10  3 18  5
				     1 12 20 15 19)
			     (board  3 15  0  2 22
				     9 18 13 17  5
				    19  8  7 25 23
				    20 11 10 24  4
				    14 21 16 12  6)
			     (board 14 21 17 24  4
				    10 16 15  9 19
				    18  8 23 26 20
				    22 11 13  6  5
				    2  0 12  3  7)))

(defparameter *numbers* '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))

(defun mark-board (b n)
  (dotimes (i (length b))
    (when (= (aref b i) n)
      (setf (aref b i) -1)
      (return-from mark-board))))

(defun board-won? (b)
  (dotimes (row 5)
    (let ((row-sum 0))
      (dotimes (col 5)
	(incf row-sum (aref b (+ (* row 5) col))))
      (when (= row-sum -5)
	(return-from board-won? t))))

  (dotimes (col 5)
    (let ((col-sum 0))
      (dotimes (row 5)
	(incf col-sum (aref b (+ (* row 5) col))))
      (when (= col-sum -5)
	(return-from board-won? t))))

  nil)

(defun score-board (b n)
  (let ((sum 0))
    (dotimes (i (length b))
      (let ((v (aref b i)))
	(unless (= v -1)
	  (incf sum v))))
    (* sum n)))

(defun mark-boards (bs ns)
  (dolist (n ns)
    (dolist (b bs)
      (mark-board b n)
      (when (board-won? b)
	(return-from mark-boards (score-board b n))))))

(defun parse-row (s)
    (with-input-from-string (in s)
      (labels ((rec (out)
		 (let ((n (read in nil)))
		   (if n
		       (rec (cons n out))
		       (nreverse out)))))
	(rec nil))))

(defun test ()
  (assert (= (mark-boards *boards* *numbers*) 4512))
  
  (with-open-file (in "~/Projects/advent/7/input")
    (let ((ns (mapcar #'parse-integer (split-sequence:split-sequence #\, (read-line in nil))))
	  (bs (labels ((read-boards (out)
			 (let ((a (read-line in nil)))
			   (if a
			       (let ((b (read-line in nil))
				     (c (read-line in nil))
				     (d (read-line in nil))
				     (e (read-line in nil)))
				 (read-line in nil)
				 (read-boards (cons (make-board :initial-contents (mapcan #'parse-row (list a b c d e)))
						    out)))
			       (nreverse out)))))
		(read-line in nil)
		(read-boards nil))))
      (format t "answer: ~a ~a ~a~%" (length ns) (length bs) (mark-boards bs ns)))))
