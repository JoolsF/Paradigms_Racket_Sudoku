#lang racket

(define jlist 
  (list (list 1 2 3) (list 2) (list 1 2 3)))

(define test-matrix 
  (list (list 0 2 5 0 0 1 0 0 0)
        (list 1 0 4 2 5 0 0 0 0)
        (list 0 0 6 0 0 4 2 1 0)
        (list 0 5 0 0 0 0 3 2 0)
        (list 6 0 0 0 2 0 0 0 9)
        (list 0 8 7 0 0 0 0 6 0)
        (list 0 9 1 5 0 0 6 0 0)
        (list 0 0 0 0 7 8 1 0 3)
        (list 0 0 0 6 0 0 5 9 0)
        )
  )




;; test 1
(define (find-singleton lst)
  (acc-find-singleton lst '()))

(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  

;;test 2
;(map (lambda (x)
;       (cond
;         [(= (length x) 1) x]
;         [else (remove* (find-singleton jlist) x)])) 
;     jlist)


;;test 3 - Traversing a 2d matrix

