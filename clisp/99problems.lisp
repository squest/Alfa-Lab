;; 99 Lisp problems

(defun inc (i)
  (1+ i))

(defun dec (i)
  (1- i))

(defun take (n ls)
  "Take n first elements of ls"
  (if (zerop n)
      nil
      (cons (first ls)
	    (take (dec n) (rest ls)))))

(defun drop (n ls)
  "Drop first n elements of ls"
  (if (zerop n)
      ls
      (drop (dec n) (rest ls))))



;; Problem no 1
(defun my-last (ls)
  (if (<= (length ls) 1)
      ls
      (my-last (rest ls))))


;; Problem no 2
(defun my-but-last (ls)
  (drop (- (length ls) 2) ls))

(defun ccount (ls)
  (if (null ls)
      0
      (inc (ccount (rest ls)))))

(defun rev (ls)
  (if (null ls)
      nil
      (append (rev (rest ls))
	      (list (first ls)))))

(defun range (&rest args)
  (cond ((= 1 (length args))
	 (loop for i from 0 to (first args)
	    collect i))
	((= 2 (length args))
	 (loop for i from (first args) to (second args)
	    collect i))
	((= 3 (length args))
	 (let ((m (first args))
	       (n (second args))
	       (o (third args)))
	   (if (< m n)
	       (loop for i from m to n by o
		  collect i)
	       (loop for i from m downto n by o
		  collect i))))))

(defun palin? (ls)
  (equal ls (rev ls)))

(defun flatten (ls)
  (cond ((null ls)
	 nil)
	((atom (first ls))
	 (cons (first ls)
	       (flatten (rest ls))))
	(t (append (flatten (first ls))
		   (flatten (rest ls))))))

(defun compress-helper (lelmt ls)
  (if (null ls)
      nil
      (if (= lelmt (first ls))
	  (compress-helper lelmt (rest ls))
	  (cons (first ls) (compress-helper (first ls) (rest ls))))))

(defun compress (ls)
  (cons (first ls)
	(compress-helper (first ls)
			 (rest ls))))

(defun pack-helper (lelmt ls res resta)
  (if (null ls)
      (rev (cons res resta))
      (if (= lelmt (first ls))
	  (pack-helper lelmt
		       (rest ls)
		       (cons lelmt res)
		       resta)
	  (pack-helper (first ls)
		       (rest ls)
		       (list (first ls))
		       (cons res resta)))))

(defun pack (ls)
  (pack-helper (first ls)
	       (rest ls)
	       (list (first ls))
	       nil))

(defun encode (ls)
  (mapcar #'(lambda (x) (list (length x) (first x)))
	  (pack ls)))

(defun encode-modified (ls)
  (mapcar #'(lambda (x) (if (= 1 (length x))
		       (first x)
		       (list (length x) (first x))))
	  (pack ls)))

(defun repeat (n e)
  (if (zerop n)
      nil
      (cons e (repeat (dec n) e))))

(defun decode (ls)
  (if (null ls)
      nil
      (if (atom (first ls))
	  (cons (first ls) (decode (rest ls)))
	  (append (repeat (first (first ls))
			  (second (first ls)))
		  (decode (rest ls))))))

(defun split (ls n)
  (list (take n ls)
	(drop n ls)))

(defun slice (ls i j)
  (take (- j (dec i)) (drop (dec i) ls)))

(defun rotate (ls n)
  (append (drop n ls)
	  (take n ls)))

(defun sort-by (f ls)
  (if (null ls)
      nil
      (let* ((smaller (remove-if #'(lambda (x) (> (funcall f x)
					  (funcall f (first ls))))
			      (rest ls)))
	     (larger (remove-if #'(lambda (x) (<= (funcall f x)
					  (funcall f (first ls))))
			     (rest ls))))
	(append (sort-by f smaller)
		(list (first ls))
		(sort-by f larger)))))

(defun prime? (n)
  (labels ((helper (i lim)
	     (if (> i lim)
		 t
		 (if (zerop (rem n i))
		     nil
		     (helper (+ 2 i) lim)))))
    (helper 3 (sqrt n))))

(defun suma-prima (lim)
  (+ 2 (apply '+ (remove-if-not 'prime? (range 3 lim 2)))))

(defun sum-primes (lim)
  (labels ((helper (i res)
	     (if (> i lim)
		 res
		 (if (prime? i)
		     (helper (+ 2 i) (+ i res))
		     (helper (+ 2 i) res)))))
    (helper 7 10)))

(defun fibo (lim)
  (labels ((helper (cur lst i)
	     (if (>= cur lim)
		 (list i cur)
		 (helper (+ cur lst) cur (+ 1 i)))))
    (helper 1 1 2)))








