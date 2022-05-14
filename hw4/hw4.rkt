
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1st function

(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

;; 2nd function

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3rd function

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t
         (define i (remainder n (length xs)))
         (define (helper xs1 n1)
           (cond [(= n1 i) (car xs1)]
                 [#t (helper (cdr xs1) (+ 1 n1))]))
         (helper xs 0)]))


;; 4th function

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([p (s)])
        (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))

;; 5th task

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 6th task

(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (odd? x)
                    (cons "dan.jpg" (lambda () (f (+ x 1))))
                    (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 7th task

(define (stream-add-zero s)
  (lambda ()
    (let ([v (s)])
      (cons (cons 0 (car v)) (stream-add-zero (cdr v))))))

;; 8th task

(define (cycle-lists xs ys)
  (define (helper-with-i i)
    (lambda ()
      (cons (cons (list-nth-mod xs i) (list-nth-mod ys i))
            (helper-with-i (+ 1 i)))))
  (helper-with-i 0))

;; 9th task

(define (vector-assoc v vec)
  (define (helper i)
    (if (>= i (vector-length vec))
        #f
        (let ([try (vector-ref vec i)])
          (if (and (pair? try) (equal? (car try) v))
              try
              (helper (+ i 1))))))
  (helper 0))

;; 10th task

(define (cached-assoc xs n)
  (define memo (make-vector n #f))
  (define ind 0)
  (lambda (v)
    (let ([try (vector-assoc v memo)])
      (if try
          try
          (let ([try1 (assoc v xs)])
            (if try1
                (begin
                  (vector-set! memo ind try1)
                  (set! ind (if (>= ind (- (vector-length memo) 1)) 0 (+ 1 ind)))
                  try1)
                try1))))))
 


     
