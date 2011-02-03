;;; Scott Pillow - p3

(define (find-not-phi-match? l)
 (let* ((not-phis
	 (filter (lambda (v) (and (list? v) (eq? (first v) 'not))) l)))
  (some (lambda (not-phi)
	 (some (lambda (item)
		(equal? (second not-phi) item)) l)) not-phis)))

(define (split-on-item f l)
 (define (go found l before splitter after)
  (if (null? l)
      (if (not found)
	  #f
	  (list (reverse before) splitter (reverse after)))
      (if (not found)
	  (if (f (first l))
	      (go #t (rest l) before (first l) after)
	      (go #f (rest l) (cons (first l) before) splitter after))
	  (go #t (rest l) before splitter (cons (first l) after)))))
 (go #f l '() 0 '()))

(define (simplify-not phi)
 (if (not (= (length phi) 2))
     (panic "not takes only one argument")
     (let ((arg (boolean-simplify (second phi))))
      (cond
       ((boolean? arg) (not arg))
       ((symbol? arg) `(not ,arg))
       ((list? arg)
	(if (eq? (first arg) 'not)
	    (boolean-simplify (second arg))
	    `(not ,arg)))
       (else phi)))))

(define (simplify-op phi base-bool sym)
 (case (- (length phi) 1)
  ((0) base-bool)
  ((1) (boolean-simplify (second phi)))
  (else
   (define redux (map boolean-simplify (rest phi)))
   (define (begins-with-sym item)
    (if (and (list? item) (not (null? item)))
	(eq? (first item) sym)
	#f))
   (cond
    ((some (lambda (v) (eq? v base-bool)) redux)
     (boolean-simplify
      `(,sym ,@(remove (lambda (v) (eq? v base-bool)) redux))))
    ((some (lambda (v) (eq? v (not base-bool))) redux) (not base-bool))
    ((some begins-with-sym redux)
     (let* ((splits (split-on-item begins-with-sym redux))
	    (before (first splits))
	    (splitter (second splits))
	    (after  (third splits)))
      (boolean-simplify `(,sym ,@before ,@(rest splitter) ,@after))))
    ((find-not-phi-match? redux) (not base-bool))
    ((not (uniq? redux)) (boolean-simplify `(,sym ,@(setify redux))))
    (else phi)))))

(define (simplify-and phi)
 (simplify-op phi #t 'and))

(define (simplify-or phi)
 (simplify-op phi #f 'or))

(define (boolean-simplify phi)
 (cond
  ((boolean? phi) phi)
  ((symbol? phi) phi)
  ((list? phi)
   (if (null? phi) (panic "empty list encountered")
       (case (first phi)
	((not) (simplify-not phi))
	((and) (simplify-and phi))
	((or)  (simplify-or  phi))
	(else  (panic "unknown operation encountered")))))))

(define (truth-tables-match? phi phi-prime)
 (let ((I (gen-truth-assignment-rows phi)))
  (equal? (bindings->truths I phi) (bindings->truths I phi-prime))))

(define (bindings->truths I phi)
 (let ((truths (map (lambda (x) (valuation-fn phi x)) I)))
  (if (= 0 (length truths))
      (list (valuation-fn phi '()))
      truths)))

;;;;;;;;;;;;;;;; p2 utils ;;;;;;;;;;;;;;;;;;;

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

(define (remove f coll)
 (filter (lambda (x) (not (f x))) coll))

(define (setify coll)
 (define (acc coll uniq-items)
  (if (null? coll)
      (reverse uniq-items)
      (if (contains? uniq-items (first coll))
	  (acc (rest coll) uniq-items)
	  (acc (rest coll) (cons (first coll) uniq-items)))))
 (acc coll '()))

(define (uniq? l)
 (if (null? l)
     #t
     (if (contains? (rest l) (first l))
	 #f
	 (uniq? (rest l)))))

(define (contains? coll item)
 (if (null? coll)
     #f
     (if (equal? (first coll) item)
	 #t
	 (contains? (rest coll) item))))
