; Scott Pillow - p4

; repeats the value v n times
(define (repeat v n)
 (if (<= n 0)
     '()
     (cons v (repeat v (- n 1)))))

(define (range n)
 (define (rangep curr)
  (if (= curr n)
      '()
      (cons curr (rangep (+ curr 1)))))
 (rangep 0))

(define (flatten coll)
 (if (list? coll)
     (apply append (map flatten coll))
     (list coll)))

(define (filter f coll)
 (if (null? coll) '()
     (if (f (first coll))
	 (cons (first coll) (filter f (rest coll)))
	 (filter f (rest coll)))))

(define (list-ith-item l i)
 (if (null? l) (panic "index out of bounds")
     (if (= i 0)
	 (first l)
	 (list-ith-item (rest l) (- i 1)))))

(define (list-set-ith-item l i v)
 (if (null? l) (panic "index out of bounds")
     (if (= i 0)
	 (cons v (rest l))
	 (cons (first l) (list-set-ith-item (rest l) (- i 1) v)))))

; returns the value at the ij-th position on the board
(define (get-ij-value b i j)
 (list-ith-item (list-ith-item b i) j))

; returns the jth column of b.
(define (get-column b j)
 (map (lambda (i) (get-ij-value b i j)) (range (length b))))

; returns the ith row of b.
(define (get-row b i)
 (list-ith-item b i))

; returns the main diagonal starting from (0,0).
(define (get-diag-left b)
 (map (lambda (i j) (get-ij-value b i j))
      (range (length b)) (range (length b))))

; returns the diagonal starting from (0,n-1).
(define (get-diag-right b)
 (map (lambda (i j) (get-ij-value b i j))
      (range (length b)) (reverse (range (length b)))))

; returns true if all the elements in l are equal.
(define (same l)
 (define (same-as-val l v)
  (if (null? l) #t
      (if (= (first l) v)
	  (same-as-val (rest l) v)
	  #f)))
 (if (null? l)
     #f
     (same-as-val l (first l))))

; given a column, row or diagonal, returns true
; if it would allow for a win.
(define (winnable-list l)
 (and (same l) (not (= (first l) 0))))

; a board is represented as a list of rows
(define (initial-board n)
 (map (lambda (v) (repeat 0 n)) (range n)))

; returns the number of the player who's turn it
; is given a board.
; X = +1
; O = -1
(define (player-board b)
 (let* ((board-nums (flatten b))
	(num-Xs (filter (lambda (v) (= v +1)) board-nums))
	(num-Os (filter (lambda (v) (= v -1)) board-nums)))
  (if (= (length num-Xs) (length num-Os))
      +1
      -1)))

; takes a board and returns the list of legal moves for the
; current player.
(define (moves b)
 (get-board-items b 0))

; takes a move and a board and returns the new board after the move
(define (make-move m b)
 (let ((player (player-board b))
       (row (first m))
       (col (second m)))
  (list-set-ith-item b row
		     (list-set-ith-item
		      (list-ith-item b row) col player))))

; same as w0(b), returns the state of winning for the current board
; +1 = X has won
;  0 = neither player has won
; -1 = O has won
(define (win b)
 (let ((just-played (if (= (player-board b) 1) -1 1))
       (rows b)
       (cols (map (lambda (col-idx) (get-column b col-idx))
		  (range (length b))))
       (left-diag  (get-diag-left b))
       (right-diag (get-diag-right b)))
  (if (or (some winnable-list rows)
	  (some winnable-list cols)
	  (winnable-list left-diag)
	  (winnable-list right-diag))
      just-played
      0)))

(define (maximize f l)
 (define (loop best-so-far l)
  (cond ((= best-so-far 1) 1)
	((null? l) best-so-far)
	(else (loop (max (f (first l)) best-so-far)
		    (rest l)))))
 (loop -1 l))

; calculates the outcome given perfect play for the input board.
(define (w* b)
 (if (or (not (= 0 (win b))) (= 0 (length (moves b))))
     (win b)
     (* (player-board b)
	(maximize (lambda (m) (* (player-board b) (w* (make-move m b))))
		  (moves b)))))

; returns the set of moves that lead to optimal play in b.
(define (m-hat b)
 (if (not (= (win b) 0))
     '()
     (filter (lambda (m) (= (w* (make-move m b)) (w* b))) (moves b))))

(define (center? b pos)
 (let* ((dim (length b))
	(row (first pos))
	(col (second pos))
	(mid (quotient dim 2)))
  (if (even? dim)
      #f
      (and (= row mid) (= col mid)))))

(define (corner? b pos)
 (let ((dim (length b))
       (row (first pos))
       (col (second pos)))
  (or (and (= row 0) (= col 0))
      (and (= row 0) (= col (- dim 1)))
      (and (= row (- dim 1)) (= col 0))
      (and (= row (- dim 1)) (= col (- dim 1))))))

; calculates the strength of a position based upon the number
; of attacking lanes it has.
; center  = 4
; corners = 3
; other   = 2
(define (position-score b pos)
 (cond
  ((center? b pos) 4)
  ((corner? b pos) 3)
  (else            2)))

; given a board return the indices of all positions that are
; equal to the given value
(define (get-board-items b sym)
 (filter (lambda (v) (not (eq? '() v)))
	 (apply append (map (lambda (row row-idx)
			     (map (lambda (item col-idx)
				   (if (= item sym) (list row-idx col-idx) '()))
				  row (range (length row))))
			    b (range (length b))))))

(define (sum l)
 (reduce + l 0))

(define (average l)
 (let ((num (length l)))
  (/ (sum l) num)))

(define (average-score l)
 (let ((len (length l)))
  (if (= len 0)
      0
      (average l))))

; heuristic that attempts to take a stab at estimating w*(b).
(define (w0~ b)
 (let* ((Xs (get-board-items b +1))
	(Os (get-board-items b -1))
	(X-avg (average-score (map (lambda (pos) (position-score b pos)) Xs)))
	(O-avg (average-score (map (lambda (pos) (position-score b pos)) Os)))
	(side-of-zero (if (> X-avg O-avg) 1 -1))
	(x1 4)
	(y1 4)
	(x2 0)
	(y2 0)
	(x3 (if (< side-of-zero 0) 0 4))
	(y3 (if (< side-of-zero 0) 4 0))
	(x X-avg)
	(y O-avg)
	(l1-numer (+ (* (- y2 y3) (- x x3)) (* (- x3 x2) (- y y3))))
	(l2-numer (+ (* (- y3 y1) (- x x3)) (* (- x1 x3) (- y y3))))
	(denom    (+ (* (- y3 y1) (- x2 x3)) (* (- x1 x3) (- y2 y3))))
	(l1pl2    (/ (+ l1-numer l2-numer) denom))
	(l3       (- 1 l1pl2)))
  (* side-of-zero l3)))

; limits search tree to a depth of k then uses static evaluator
; if it gets to the bottom.
(define (w~ k b)
 (cond
  ((or (not (= 0 (win b))) (= 0 (length (moves b)))) (win b))
  ((<= k 0) (w0~ b))
  (else
   (* (player-board b)
      (maximize (lambda (m) (* (player-board b) (w~ (- k 1) (make-move m b))))
		(moves b))))))

; set of moves to make based upon heuristic.
(define (m~ k b)
 (if (not (= 0 (win b)))
     '()
     (let* ((poss-moves (moves b))
	    (good-moves (filter (lambda (m)
				 (>= (* (player-board b) (w~ (- k 1) (make-move m b)))
				     (* (player-board b) (w~ k b))))
				poss-moves)))
      (if (eq? '() good-moves)
	  poss-moves
	  good-moves))))

(define (optimal-moves~ k b)
 (if (= k Infinity)
     (m-hat b)
     (m~ k b)))
