					;Scott Pillow

					;takes two lists as inputs and returns a list that
					;is the union of the inputs.
(define (set-union a b)
 (setify (append a b)))

					;takes two lists as inputs and returns a list that
					;is the intersection of the inputs.
(define (set-intersection a b)
 (setify (get-both a b)))

					;takes two lists as inputs and returns a list that
					;has items in a not in b.
(define (set-minus a b)
 (setify (a-not-in-b a b)))

					;gets items in a not in b (allows duplicates).
(define (a-not-in-b a b)
 (define (acc coll accum)
  (if (empty? coll)
      accum
      (if (contains? b (first coll))
	  (acc (rest coll) accum)
	  (acc (rest coll) (cons (first coll) accum)))))
 (acc a '()))

					;generates a list of items in both lists
					;as we walk the first list (can be duplicates).
(define (get-both a b)
 (define (acc coll both)
  (if (empty? coll)
      both
      (if (contains? b (first coll))
	  (acc (rest coll) (cons (first coll) both))
	  (acc (rest coll) both))))
 (acc a '()))

					;takes a list and converts it into a set
					;(just a list with no dups).
(define (setify coll)
 (define (acc coll uniq-items)
  (if (empty? coll)
      uniq-items
      (if (contains? uniq-items (first coll))
	  (acc (rest coll) uniq-items)
	  (acc (rest coll) (cons (first coll) uniq-items)))))
 (acc coll '()))

(define empty? null?)

					;returns true if item is a member of coll.
(define (contains? coll item)
 (if (empty? coll)
     #f
     (if (= (first coll) item)
	 #t
	 (contains? (rest coll) item))))
