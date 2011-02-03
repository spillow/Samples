;;; Scott Pillow - p2

;;; (truth-table '(or #f (not (and p (or q n)))) ==>
(define (print str)
 (display str)
 (newline))

(define (print-table phi)
 (define (taker items)
  (if (null? items) "done"
      (begin
       (print (first items))
       (taker (rest items)))))
 (let ((table (truth-table phi)))
  (taker table)))

(define (truth-table phi)
 (let* ((rows   (gen-truth-assignment-rows phi))
        (truths (map (lambda (x) (valuation-fn phi x)) rows)))
  (if (eq? 0 (length rows))
      (list (list (list (list 'x #f)) (valuation-fn phi '())))
      (zipper rows truths))))

(define (valuation-fn phi I)
 (cond
  ((symbol? phi) (lookup-binding I phi))
  ((boolean? phi) phi)
  ((list? phi)
   (if (null? phi)
       (panic "unexpected empty list encountered")
       (case (first phi)
	((not) (not (valuation-fn (second phi) I)))
	((and) (reduce (lambda (acc y) (and acc y))
		       (map (lambda (x) (valuation-fn x I))
			    (rest phi)) #t))
	((or)  (reduce (lambda (acc y) (or acc y))
		       (map (lambda (x) (valuation-fn x I))
			    (rest phi)) #f)))))))

(define (lookup-binding bindings var)
 (if (null? bindings)
     (panic "couldn't find binding")
     (if (eq? (first (first bindings)) var)
	 (second (first bindings))
	 (lookup-binding (rest bindings) var))))

;;; (gen-perms '(#t #f) 3) ==>
;;; (
;;; (#f #f #f)
;;; (#f #f #t)
;;; (#f #t #f)
;;; ...
;;; )
(define (gen-perms items len)
 (case len
  ((0) '())
  ((1) (map list items))
  (else
   (reduce append
	   (map (lambda (y)
		 (map (lambda (x) (cons y x))
		      (gen-perms items (- len 1)))) items) '()))))

;;; (get-props '(and p (or q (and t #f)) a b c p (or t d))) ==>
(define (get-props phi)
 (setify (filter symbol? (visit-leaves phi))))

(define (gen-truth-assignment-rows phi)
 (let* ((props (get-props phi))
	(perms (gen-perms '(#f #t) (length props))))
  (map (lambda (x) (zipper props x)) perms)))

(define (visit-leaves l)
 (cond
  ((list? l) (if (null? l) '() (apply append (map visit-leaves (rest l)))))
  (else (list l))))

;;; (zipper '(1 2 3) '(4 5 6)) ==> ((1 4) (2 5) (3 6))
(define (zipper a b)
 (if (or (null? a) (null? b))
     '()
     (cons (list (first a) (first b)) (zipper (rest a) (rest b)))))

(define (filter f coll)
 (if (null? coll) '()
     (if (f (first coll))
	 (cons (first coll) (filter f (rest coll)))
	 (filter f (rest coll)))))

(define (setify coll)
 (define (acc coll uniq-items)
  (if (null? coll)
      (reverse uniq-items)
      (if (contains? uniq-items (first coll))
	  (acc (rest coll) uniq-items)
	  (acc (rest coll) (cons (first coll) uniq-items)))))
 (acc coll '()))

(define (contains? coll item)
 (if (null? coll)
     #f
     (if (eq? (first coll) item)
	 #t
	 (contains? (rest coll) item))))
