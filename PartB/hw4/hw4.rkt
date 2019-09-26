
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; 5
(define funny-number-stream
  (letrec ([sign (lambda (x) (if (= (remainder x 5) 0)
                                 (- 0 x)
                                 x))]
           [f (lambda (x) (cons (sign x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

;; 7
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

;; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)))]
           [g (lambda (n) (cons (f n) (lambda () (g (+ n 1)))))])
    (lambda () (g 0))))

;; 9
(define (vector-assoc v vec)
  (letrec([f (lambda (n) (cond [(= n (vector-length vec)) #f]
                               [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                               [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                               [#t (f (+ n 1))]))])
    (f 0)))

;; 10
(define (cached-assoc lst n)
  (let ([cache (make-vector n #f)]
        [next-to-replace 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v lst)])
            (and ans
                 (begin (vector-set! cache next-to-replace ans)
                        (set! next-to-replace 
                              (if (= (+ next-to-replace 1) n)
                                  0
                                  (+ next-to-replace 1)))
                        ans)))))))

;; 11
(define-syntax while-less
  (syntax-rules (do)
    ((while-less e1 do e2)
     (letrec ([e e1]
              [f (lambda () (if (< e2 e) (f) #t))])
       (f)))))
