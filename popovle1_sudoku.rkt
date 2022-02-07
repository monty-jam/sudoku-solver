#lang racket

(define (my-len lst)
  (my-len-aux lst 0))
(define (my-len-aux lst acc)
  (if (null? lst)
      acc
      (my-len-aux (cdr lst) (+ 1 acc))))

(define (my-range a b)
  (if (> a b)
      null
      (cons a (my-range (+ a 1) b))))

(define (my-fake-sqrt n)
  (my-fake-sqrt-aux n 1))
(define (my-fake-sqrt-aux n i)
  (if (<= n (* i i))
      i
      (my-fake-sqrt-aux n (+ i 1))))