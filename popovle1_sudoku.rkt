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

(define (my-list-ref lst i)
  (if (= i 1)
      (car lst)
      (my-list-ref (cdr lst) (- i 1))))

(define (my-get-row mtx y)
  (my-list-ref mtx y))

(define (my-get-column mtx x)
  (map (lambda (lst) (my-list-ref lst x)) mtx))

(define (my-get-cell mtx x y)
  (my-list-ref (my-get-row mtx y) x))

(define mtx-test
  '((4 0 1 6 0 8 3 0 0)
    (6 0 0 1 9 3 5 0 0)
    (3 9 8 0 2 0 0 0 0)
    (7 0 0 8 0 0 0 2 0)
    (0 0 5 0 3 6 9 8 7)
    (0 8 0 0 1 2 0 6 5)
    (0 3 9 2 0 0 8 1 6)
    (8 4 0 0 6 1 0 5 9)
    (1 7 0 9 0 0 0 0 4)))