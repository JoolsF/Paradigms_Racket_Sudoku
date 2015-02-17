#lang racket

(define jlist 
  (list (list 1 2 3) (list 2) (list 1 2 3)))



;; test 1
(define (find-singleton lst)
  (acc-find-singleton lst '()))

(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  

;;test 2
(map (lambda (x)
       (cond
         [(= (length x) 1) x]
         [else (remove* (find-singleton jlist) x)])) 
     jlist)


;;test 3 - mapping a nested list structure

