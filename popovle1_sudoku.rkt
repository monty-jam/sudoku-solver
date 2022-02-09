#lang racket

(define (my-foldr lst foo init)
  (if (null? lst)
    init
    (foo (car lst) (my-foldr (cdr lst) foo init))))

(define (my-foldl lst foo init)
  (if (null? lst)
      init
      (my-foldl (cdr lst) foo (foo init (car lst)))))

(define (my-filter lst foo)
    (my-foldr lst (lambda (e res) (if (foo e)
                                      (cons e res)
                                      res))
              null))

(define (my-append lst1 lst2)
  (my-foldr lst1 cons lst2))
  
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

(define (fake-sqrt n)
  (fake-sqrt-aux n 1))
(define (fake-sqrt-aux n i)
  (if (<= n (* i i))
      i
      (fake-sqrt-aux n (+ i 1))))

(define (list-cut-head lst i)
  (if (= i 1)
      lst
      (list-cut-head (cdr lst) (- i 1))))

(define (list-cut-tail lst i)
  (if (= i 1)
      (cons (car lst) null)
      (cons (car lst) (list-cut-tail (cdr lst) (- i 1)))))

(define (list-cut lst l r)
  (list-cut-head (list-cut-tail lst r) l))

(define (list-ref lst i)
  (if (= i 1)
      (car lst)
      (list-ref (cdr lst) (- i 1))))

(define (get-row mtx y)
  (list-ref mtx y))

(define (get-column mtx x)
  (map (lambda (lst) (list-ref lst x)) mtx))

(define (get-cell mtx x y)
  (list-ref (get-row mtx y) x))

(define (find-box-range a size acc)
  (if (<= a (+ acc size))
      (cons (+ acc 1) (+ acc size))
      (find-box-range a size (+ acc size))))

(define (get-box mtx x y size)
  (let ([rows (find-box-range y size 0)]
        [columns (find-box-range x size 0)])
    (map (lambda (row) (list-cut row (car columns) (cdr columns)))
         (list-cut mtx (car rows) (cdr rows)))
  )
)

(define (append-lists lst)
  (my-foldl lst my-append null))

(define (is-in-list val lst)
  (cond
    ((null? lst) #f)
    ((= (car lst) val) #t)
    (#t (is-in-list val (cdr lst)))))

; returns a list of values from lst1, that have not been occured in lst2
(define (left-outer-join lst1 lst2)
  (my-filter lst1 (lambda (x) (not (is-in-list x lst2)))))
  
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