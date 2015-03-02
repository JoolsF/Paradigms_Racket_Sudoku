#lang racket

(require racket/include)
(include "data.rkt")
(include "matrix-helper.rkt")


;; FIND SINGLETONS AND GATHERS THEM INTO SINGLE LIST
;; Takes a 2d list of lists and returns the contents of all singletons as a list 
(define (find-singleton lst)
  (acc-find-singleton lst '()))

(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  


;;Finds singleton in column
(define (find-singleton-column item-no matrix)
  (find-singleton 
   (map (lambda (x) (get-item item-no x)) matrix)))


;; SETS UP THE SUDOKU  
;;Tranforms matrix by replacing all zeroes in a list with the (list 1-9) and all other values as a singleton list
(define (transform matrix)
  (deep-map-innermost (lambda (x) 
              (cond
                [(= x 0)(list 1 2 3 4 5 6 7 8 9) ]
                [else (list x)])) 
            matrix))

;;SOLVES THE SUDOKU
(define (filter-rows matrix)
  (map (lambda (x) 
         (filter-rows-helper x)) 
       matrix))

(define (filter-rows-helper matrix)
  (map (lambda (x)
         (cond
           [(= (length x) 1) x]
           [else (remove* (find-singleton matrix) x)]))
         matrix))





(define (filter-columns2 indexed-matrix matrix) 
  (append(map         
      (lambda (x)    (remove-if-non-singleton (find-singleton-column (cadr x) matrix) (car x)))
  indexed-matrix)))

(define (filter-columns matrix)
  (append
   (map 
    (lambda (x) (filter-columns2 x matrix))
    (add-column-index matrix)
    )
   )
  )



;;EXECUTE HERE
(filter-columns (filter-rows(transform matrix)))



;;;NOTES TO DELETE

;  once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix
;  http://stackoverflow.com/questions/15871042/how-do-i-find-the-index-of-an-element-in-a-list-in-racket

;;END OF NOTES TO DELETE