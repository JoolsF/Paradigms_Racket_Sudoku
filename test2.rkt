#lang racket

(provide jlist)

;deep-map function taken from  http://stackoverflow.com/questions/5699899/scheme-map-function-for-applying-a-function-to-elements-in-a-nested-list
; worked out concept of mapping a map until the inner element is discovered by couldn't find optimal way of calling it
(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))


(define jlist 
  (list (list 1 2 3) (list 2) (list 1 2 3)))
      

(define matrix 
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

;; The set {1-9}
(define sudokuset(list 1 2 3 4 5 6 7 8 9))





;;replaces zeroes in list with sudokuset
(define (replace-empty x)
  (if (= x 0)
      sudokuset
      (list x))
  )



