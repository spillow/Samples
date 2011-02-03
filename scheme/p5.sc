; Scott Pillow - p5

(define (range n)
 (define (rangep curr)
  (if (= curr n)
      '()
      (cons curr (rangep (+ curr 1)))))
 (rangep 0))

(define (my-reduce f coll init)
 (define (mreduce f coll acc)
  (if (null? coll)
      acc
      (mreduce f (rest coll) (f acc (first coll)))))
 (mreduce f coll init))

(define filter remove-if-not)

(define (can-attack? q1 q2)
 (let ((r1 (first q1))
       (r2 (first q2))
       (c1 (second q1))
       (c2 (second q2)))
  (and (or (= r1 r2)
	   (= c1 c2)
	   (= (abs (- r1 r2)) (abs (- c1 c2))))
       (or (not (= r1 r2)) (not (= c1 c2))))))

(define (place-given-board list-of-queens curr-row N)
 (let ((curr-col (an-integer-between 0 (- N 1))))
  (when (some (lambda (q)
	       (can-attack? q (list curr-row curr-col))) list-of-queens)
   (fail))
  (place-queen curr-row curr-col)
  (cons (list curr-row curr-col) list-of-queens)))

(define (place-n-queens-by-backtracking N)
 (my-reduce (lambda (board row)
	     (place-given-board board row N)) (range N) '()))

(define (assert-unary-constraint-gfc! constraint x)
 (let ((sat-domain-subset (filter constraint (domain-variable-domain x))))
  (restrict-domain! x sat-domain-subset)))

(define (assert-binary-constraint-gfc! constraint x y)
 (define (gfc-demon x y)
  (attach-after-demon!
   (lambda ()
    (when (bound? x)
     (restrict-domain! y
		       (filter (lambda (v) (constraint (binding x) v))
			       (domain-variable-domain y)))))
   x))
 (gfc-demon x y)
 (gfc-demon y x))

(define (assert-unary-constraint-ac! constraint x)
 (let ((sat-domain-subset (filter constraint (domain-variable-domain x))))
  (restrict-domain! x sat-domain-subset)))

(define (assert-binary-constraint-ac! constraint x y)
 (define (ac-demon x y)
  (attach-after-demon!
   (lambda ()
    (restrict-domain!
     y
     (filter (lambda (this)
	      (some (lambda (other)
		     (constraint this other))
		    (domain-variable-domain x)))
	     (domain-variable-domain y))))
   x))
 (ac-demon x y)
 (ac-demon y x))

(define (place-n-queens-by-constraints n)
 (let ((variables
	(map (lambda (i) (create-domain-variable (range n))) (range n))))
  (for-each
   (lambda (v i)
    (attach-after-demon!
     (lambda ()
      (when (bound? v) (place-queen i (binding v))))
     v))
   variables (range (length variables)))
  (for-each (lambda (variable-1 i)
	     (for-each (lambda (variable-2 j)
			(assert-constraint!
			 (lambda (c1 c2)
			  (not (can-attack? (list i c1) (list j c2))))
			 (list variable-1 variable-2)))
		       variables (range (length variables))))
	    variables (range (length variables)))
  (csp-solution variables first)))
