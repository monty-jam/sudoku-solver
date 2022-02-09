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

(define (my-list-cut-head lst i)
  (if (<= i 1)
      lst
      (my-list-cut-head (cdr lst) (- i 1))))

(define (my-list-cut-tail lst i)
  (if (<= i 1)
      (cons (car lst) null)
      (cons (car lst) (my-list-cut-tail (cdr lst) (- i 1)))))

(define (my-list-cut lst l r)
  (if (> l r)
      null
      (my-list-cut-head (my-list-cut-tail lst r) l)))

(define (my-list-ref lst i)
  (if (= i 1)
      (car lst)
      (my-list-ref (cdr lst) (- i 1))))

(define (get-row mtx y)
  (my-list-ref mtx y))

(define (get-column mtx x)
  (map (lambda (lst) (my-list-ref lst x)) mtx))

(define (get-cell mtx x y)
  (my-list-ref (get-row mtx y) x))

(define (find-box-range a size acc)
  (if (<= a (+ acc size))
      (cons (+ acc 1) (+ acc size))
      (find-box-range a size (+ acc size))))

(define (get-box mtx x y size)
  (let ([rows (find-box-range y size 0)]
        [columns (find-box-range x size 0)])
    (map (lambda (row) (my-list-cut row (car columns) (cdr columns)))
         (my-list-cut mtx (car rows) (cdr rows)))))

(define (append-lists lst)
  (my-foldl lst my-append null))

(define (is-in-list val lst)
  (cond
    [(null? lst) #f]
    [(= (car lst) val) #t]
    [#t (is-in-list val (cdr lst))]))

; returns a list of values from lst1, that have not been occured in lst2
(define (left-outer-join lst1 lst2)
  (my-filter lst1 (lambda (x) (not (is-in-list x lst2)))))

; returns coordinates of empty cells in a given row
(define (empty-cells-row lst x y)
  (if (null? lst)
      null
      (if (= (car lst) 0)
          (cons (cons x y) (empty-cells-row (cdr lst) (+ x 1) y))
          (empty-cells-row (cdr lst) (+ x 1) y))))

; returns a sequence of coordinates (pair of atoms) of empty cells
(define (empty-cells mtx range)
  (append-lists
    (map (lambda (lst y) (empty-cells-row lst 1 y)) mtx range)))

(define (my-list-set lst x val)
  (let ([size (my-len lst)])
    (let ([i (+ (- size x) 1)]) ; invert x coordinate
      (my-list-set-aux lst i val size))))
(define (my-list-set-aux lst x val size)
  (if (= size 0)
      null
      (if (= size x)
          (cons val (my-list-set-aux (cdr lst) x val (- size 1)))
          (cons (car lst) (my-list-set-aux (cdr lst) x val (- size 1))))))

; returns a new matrix with changed value in given coordinates
(define (set-cell mtx x y val)
  (my-list-set mtx y (my-list-set (my-list-ref mtx y) x val)))
  
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