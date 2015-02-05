
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let [(rem (remainder n (length xs)))]
              (car (list-tail xs rem)))]))

;; Problem 4

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let* ([pr (s)]
             [result (car pr)]
             [stream (cdr pr)])
        (cons result (stream-for-n-steps stream (- n 1))))))

;; Problem 5

(define funny-number-stream
  (letrec 
      ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x) 
                            (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Problem 6

(define dan-then-dog
  (letrec 
      ([f (lambda (x) (cons x
                            (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;; Problem 7

(define (stream-add-zero s)
  (letrec
      ([f (lambda (new)
            (let ([pr (new)])
              (cons (cons 0 (car pr)) (lambda () (f (cdr pr))))))])
    (lambda () (f s))))

;; Problem 8

(define (cycle-lists xs ys)
  (letrec 
      ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                            (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; Problem 9

(define (vector-assoc v vec)
  (letrec ([f (lambda (i)
               (cond [(= i (vector-length vec)) #f]
                     [(not (pair? (vector-ref vec i))) (f (+ i 1))]
                     [(equal? (car (vector-ref vec i)) v) (vector-ref vec i)]
                     [#t (f (+ i 1))]))])
    (f 0)))

;; Problem 10

(define (cached-assoc xs n)
  (define (bump x) (if (= x (- n 1)) 0 (+ x 1)))
  (define vec (make-vector n #f))
  (define place 0)
  (lambda (v)
    (let ([res (vector-assoc v vec)])
      (if (not res)
          (let ([val (assoc v xs)])
            (if (pair? val)
                (begin
                 ;; (printf "vector setting: ~a ~a ~a" vec place val)
                  (vector-set! vec place val)
                  (set! place (bump place))
                  val)
                #f))
          res))))
                  
   