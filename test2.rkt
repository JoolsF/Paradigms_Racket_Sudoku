#lang racket

(require racket/include)
(include "data.rkt")
(include "test2.rkt")


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




; Helper method to get nth item of list
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



;;TEST AREA


(define (remove-if-non-singleton lst-to-remove lst)
  (if (> (length lst) 1) 
      (remove* lst-to-remove lst)
      lst)) 



; TO DO - refactor
(define (zip p q) (map list p q)) 

(define (find-singleton-column item-no matrix)
  (find-singleton (map (lambda (x) (get-item item-no x)) matrix)))




(define (remove-col-dupe matrix)
  (deep-map-innermost-list (lambda (x) 
                             (remove-if-non-singleton (find-singleton-column 0 matrix) x))
                           matrix))
       


(define jools (map (lambda (x) (zip x (list 0 1 2 3 4 5 6 7 8) )) (transform matrix))) ;<-- gives the index

;(transform matrix)

 ;;TEST AREA

;(define (rebuild jmatrix)
;  (append (map (lambda (row) (rebuild-row jmatrix row '() 0)) jmatrix) '()))
  ;(rebuild-row jmatrix '() 0))


  
;(define (rebuild-row jmatrix row acc index)
  ;(cond
 ; [(empty? row) acc]
  ;[(rebuild-row (cdr row) acc (+ index 1)) (append acc (append (car row) (list " INDEX --> ") index))]))
  ;[(rebuild-row (cdr row) acc (+ index 1)) (print(append acc (append (car row) (list " INDEX --> ") index)))]))
;[(append (rebuild-row jmatrix (cdr row) acc (+ index 1))  acc (remove-if-non-singleton (find-singleton-column index jmatrix) row))]))

 






;;for each row, rebuild with  remove-if-non-singleton (find-singleton-column index matrix) row



;(rebuild (solve(transform matrix)))



;;END OF TEST AREA

;; PROGRAM EXCECUTION

;(find-singleton-column 0 (solve(transform matrix)))



;(length (solve (transform matrix)))

;(remove-col-dupe (solve(transform matrix)))


;;;NOTES TO DELETE
; (member "fall" (list "hop" "skip" "jump"))
;  once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix
;  http://stackoverflow.com/questions/15871042/how-do-i-find-the-index-of-an-element-in-a-list-in-racket
; (list-index (curry equal? 2) (car (transform matrix)))
;;END OF NOTES TO DELETE