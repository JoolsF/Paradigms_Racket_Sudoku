#lang racket

(define jlist 
  (list (list 1 2 3) (list 2) (list 1 2 3)))


(define (find-singleton lst resLst)
  (cond
    [(empty? resLst) ]
    [(= (length (car lst)) 1) (find-singleton (cdr lst) (append resLst (car lst)))])
  (printr 
  )



(find-singleton jlist '())