#!/usr/bin/env racket
#lang planet neil/sicp

;; To interactively play in repl
;; > (require racket/enter)
;; > (enter! "file.rkt")

"Hello SICP!"

;; Newton's method of square root approximation
;; ======

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-recur guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-recur (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (sqrt x)
  (sqrt-recur 1.0 x))


;; Newton's method of cube root approximation 

(define (cube-root-recur guess x)
  (if (< (abs (- (* guess guess guess) x)) 0.001)
    guess
    (cube-root-recur (/ (+ (/ x (* guess guess)) (* 2 guess)) 3) x)))

(define (cube-root x)
  (cube-root-recur 1.0 x))


;; Chapter 2 Data abstractions

;; Exercise 2.1
;; Make rational numbers with probable negative integer
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ n g -1) (/ d g -1))
        (cons (/ n g )   (/ d g)))))

;; Exercise 2.4
;; Cons pair with lambda

(define (cons x y)
  (lambda (f) (f x y) ))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; Exercise 2.5
;; Cons pair of integers with power

; raise base to n-th power
(define (power-25 base n)
  (if (<= n 0)
      1
      (* base (power-25 base (dec n)))))


; inverse of power-25
(define (depower-25 base n)
  (if (= (remainder n base) 0)
      (inc (depower-25 base (quotient n base)))
      0))

(define (cons-25 x y)
  (* (power-25 2 x) (power-25 3 y)))

(define (car-25 z)
  (depower-25 2 z))

(define (cdr-25 z)
  (depower-25 3 z))


;; Exercise 2.6 Church numeral

(define zero
  (lambda (f) (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define (inc-26 z)
  (lambda (f)
    (lambda (x) (f ((z f) x)))))

(define (plus n m)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))
