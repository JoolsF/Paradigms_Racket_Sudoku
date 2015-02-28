#lang racket


(require racket/include)
(include "data.rkt")


;; HELPER FUNCTIONS - To move other module
;; deep-map taken from http://stackoverflow.com/questions/5699899/scheme-map-function-for-applying-a-function-to-elements-in-a-nested-list
;; as can be seen from github history, I'd been using a process of nested maps to iterate a 2d list but needed an efficient way of abstracting this
;; to avoid code repetition and to increase readability
(define (deep-map-innermost f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))


; TO DO - refactor 
;Applies function to innermost lists of a matrix
(define (deep-map-innermost-list f l)
  (let deep ((x l))
    (cond [(null? x) x]
          [(and (pair? x) (number? (car x)))
           (f x)]
          (else (map deep x)))))






(define (get-item number list)                   
      (cond 
        [(null? list) '()]             
        [(= number 0) (car list)]
        [else (get-item (- number 1) (cdr list))]
        )
  )

;; END OF HELPER FUNCTIONS


;; Tranforms matrix by replacing all zeroes in a list with the (list 1-9) and all other values as a singleton list
(define (transform matrix)
  (deep-map-innermost (lambda (x) 
              (cond
                [(= x 0)(list 1 2 3 4 5 6 7 8 9) ]
                [else (list x)])) 
            matrix))



;;takes a list of lists and returns the contents of all singletons as a list 
(define (find-singleton lst)
  (acc-find-singleton lst '()))

(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  
(define (remove-singleton-from-sets matrix)
  (map (lambda (x)
         (cond
           [(= (length x) 1) x]
           [else (remove* (find-singleton matrix) x)]))
         matrix))

(define (solve matrix)
  (map (lambda (x) 
         (remove-singleton-from-sets x)) 
       matrix))



;; PROGRAM EXCECUTION
;(solve (transform matrix))



(define (iterate-col matrix)
  (deep-map-innermost-list (lambda (x) (print x)) 
                         matrix))


;(get-item 1 (car(transform matrix)))


;(map (lambda (x) (get-item 1 x)) (transform matrix))


(find-singleton (get-item 0 (transform matrix)))

;;;NOTES TO DELETE
; (member "fall" (list "hop" "skip" "jump"))
;  once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix
;  http://stackoverflow.com/questions/15871042/how-do-i-find-the-index-of-an-element-in-a-list-in-racket
; (list-index (curry equal? 2) (car (transform matrix)))
;;END OF NOTES TO DELETE