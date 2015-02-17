#lang racket

;(member "fall" (list "hop" "skip" "jump"))



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

(define sudokuset(list 1 2 3 4 5 6 7 8 9))


;;replaces empty space in matrix, i.e. zeroes, with sudokuset
(define (replace-empty x)
  (if (= x 0)
  sudokuset
  (list x)))


;; mapping nested each element in matrix to function
;; refactor to do in one statement so don't have to call like.
;;(map (lambda (x) (map-matrix replace-empty x)) matrix)
(define (map-matrix f lst)
  (map (lambda (x) (f x)) lst))






;; Execution starts here
;; (solve matrix)
;; (transform matrix)
;; once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix

;(map (lambda (x) (map-matrix replace-empty x)) matrix) ; replaces zeroes with {1-9} this should be replace by (transform matrix)

(define (transform matrix) 
  (append matrix (list 1)))



