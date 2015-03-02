#lang racket

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

(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))

(define (transform matrix)
  (deep-map (lambda (x) 
              (cond
                [(= x 0)(list 1 2 3 4 5 6 7 8 9) ]
                [else (list x)])) 
            matrix))


(define (zip p q) (map list p q))
(define (add-column-index matrix)
  (map (lambda (x) 
         (zip x (list 0 1 2 3 4 5 6 7 8) ))
       matrix))  ;<-- gives the index


(define (get-index acc)
  (for* ([i (in-range 9)]
       [j (in-range 9)])
    (append acc (cons i j)))
  acc
  )

;00 01 02 03 04 05 06 07 08
;10 11 12 13 14 15 16 17 18

