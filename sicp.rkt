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

;; Alyssa's interval arithmetic
;; Exercise 2.7

(define (make-interval m M)
  (cons m M))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Exercise 2.11
; x0 = (lower-bound x), x1 = (upper-bound x)
; 1: x0 >= 0, 0: x1 >= 0 and x0 < 0, -1: x1 < 0
; x, y:
; 1, 1: x0 * y0, x1 * y1
; 1, 0:
; 1, -1:
; 0, 1:
; 0, 0: (min (* x0 y1) (* x1 y0)), (max (* x0 y0) (* x1 y1))
; ...
(define (mul-interval2 x y)
  (cond ((>= (lower-bound x) 0)
         (cond ((>= (lower-bound y) 0)
                (make-interval (* (lower-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((>= (upper-bound y) 0)
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               (else
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y))))))
        ((>= (upper-bound x) 0)
         (cond ((>= (lower-bound y) 0)
                (mul-interval2 y x))
               ((>= (upper-bound y) 0)
                (make-interval (min (* (lower-bound x) (upper-bound y))
                                    (* (upper-bound x) (lower-bound y)))
                               (max (* (lower-bound x) (lower-bound y))
                                    (* (upper-bound x) (upper-bound y)))))
               (else
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (lower-bound y))))))
        (else
         (cond ((< (upper-bound y) 0)
                (make-interval (* (upper-bound x) (upper-bound y))
                               (* (lower-bound x) (lower-bound y))))
               (else
                (mul-interval2 y x))))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound x)))))

;; Exercise 2.8 interval subtraction
;  Exercise 2.10
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9 width of interval
(define (interval-width z)
  (/ (- (upper-bound z)
        (lower-bound z))
     2.0))

; (+ (interval-width x) (interval-width y))
; => (+ (/ (- (upper-bound x) (lower-bound x)) 2.0)
;    (/ (- (upper-bound y) (lower-bound y)) 2.0))
; => (/ (+ (- (upper-bound x) (lower-bound x))
;       (- (upper-bound y) (lower-bound y)) )
;    2.0)
; => (/ (- (+ (upper-bound x) (upper-bound y))
;       (+ (lower-bound x) (lower-bound y)))
;    2.0)
; => (interval-width (add-interval x y))

; (interval-width (sub-interval x y))
; => (interval-width (make-interval (- (lower-bound x) (upper-bound y))
;                                   (- (upper-bound x) (lower-bound y))))
; => (/ (- (- (upper-bound x) (lower-bound y))
;          (- (lower-bound x) (upper-bound y))) 2)
; => (/ (+ (- (upper-bound x) (lower-bound x))
;          (- (upper-bound y) (lower-bound x))) 2)
; => (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;       (/ (- (upper-bound y) (lower-bound x)) 2))
; => (+ (interval-width x) (interval-width y))

