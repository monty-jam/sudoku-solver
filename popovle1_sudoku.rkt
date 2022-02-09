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

(define (get-box mtx x y)
  (let ([size (fake-sqrt (my-len mtx))])
    (let ([rows (find-box-range y size 0)]
          [columns (find-box-range x size 0)])
      (map (lambda (row) (my-list-cut row (car columns) (cdr columns)))
           (my-list-cut mtx (car rows) (cdr rows))))))

(define (append-lists lst)
  (my-foldl lst my-append null))

; returns a list of unavailable values in current cell
(define (get-neighbours mtx x y)
  (append-lists (list (get-row mtx y) (get-column mtx x) (append-lists (get-box mtx x y)))))
  
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
(define (empty-cells mtx)
  (append-lists
    (map (lambda (lst y) (empty-cells-row lst 1 y)) mtx (my-range 1 (my-len mtx)))))

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
(define (mtx-set mtx x y val)
  (my-list-set mtx y (my-list-set (my-list-ref mtx y) x val)))

; tree recursive main function for solving sudoku
(define (solve-sudoku mtx)
  (solve-sudoku-step mtx (empty-cells mtx) 0))
       
(define (solve-sudoku-step mtx seq val)
  (if (null? seq)
      mtx
      (let ([newseq (cdr seq)]
            [newvals (left-outer-join (my-range 1 (my-len mtx))
                                      (get-neighbours mtx (car (car seq)) (cdr (car seq))))])
            (if (null? newvals)
                '()
                (append-lists
                  (map
                  (lambda (val) (solve-sudoku-step (mtx-set mtx (car (car seq)) (cdr (car seq)) val)
                                                   newseq
                                                   val))
                  newvals))))))

; sudoku 9x9
(define mtx1
  '((4 0 1 6 0 8 3 0 0)
    (6 0 0 1 9 3 5 0 0)
    (3 9 8 0 2 0 0 0 0)
    (7 0 0 8 0 0 0 2 0)
    (0 0 5 0 3 6 9 8 7)
    (0 8 0 0 1 2 0 6 5)
    (0 3 9 2 0 0 8 1 6)
    (8 4 0 0 6 1 0 5 9)
    (1 7 0 9 0 0 0 0 4)))

; sudoku 4x4
(define mtx2
  '((3 4 1 0)
    (0 2 0 0)
    (0 0 2 0)
    (0 1 4 3)))

; sudoku 1x1 with an empty cell
(define mtx3
  '((0)))

; sudoku 1x1 without empty cells
(define mtx4
  '((1)))

; sudoku 16x16
(define mtx5
  '((12 0 0 0 3 0 0 4 0 0 0 14 0 8 6 1)
    (5 0 6 0 0 0 16 8 9 12 0 2 0 0 0 0)
    (2 0 15 0 0 6 0 0 7 13 4 0 0 16 14 0)
    (0 0 0 0 10 7 14 2 8 15 6 1 0 0 0 0)
    (8 11 0 7 0 0 0 3 10 0 0 0 13 0 0 12)
    (0 0 12 1 0 0 0 9 0 3 0 0 14 11 0 0)
    (0 14 3 5 0 8 4 0 1 11 0 0 6 0 7 0)
    (0 6 2 13 7 0 10 0 0 0 8 12 1 0 5 3)
    (6 3 0 2 9 12 0 0 0 1 0 4 10 14 11 0)
    (0 16 0 11 0 0 13 15 0 6 10 0 8 12 9 0)
    (0 0 10 15 0 0 3 0 14 0 0 0 7 6 0 0)
    (7 0 0 9 0 0 0 6 13 0 0 0 15 0 2 16)
    (0 0 0 0 4 5 15 10 11 7 9 8 0 0 0 0)
    (0 15 1 0 0 9 8 7 0 0 16 0 0 13 0 14)
    (0 0 0 0 12 0 6 13 2 10 0 0 0 15 0 8)
    (10 8 5 0 16 0 0 0 12 0 0 13 0 0 0 7)))

; sudoku without a solution
(define mtx6
  '((4 2 1 6 0 8 3 0 0)
    (6 0 0 1 9 3 5 0 0)
    (3 9 8 0 2 0 0 0 0)
    (7 0 0 8 0 0 0 2 0)
    (0 0 5 0 3 6 9 8 7)
    (0 8 0 0 1 2 0 6 5)
    (0 3 9 2 0 0 8 1 6)
    (8 4 0 0 6 1 0 5 9)
    (1 7 2 9 0 0 0 0 4)))


  