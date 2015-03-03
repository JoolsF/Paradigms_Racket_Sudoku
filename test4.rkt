#lang racket

(require racket/include)
(include "data.rkt")

;;;; HELPER METHODS - Move to another file ;;;;

;  Takes a list of lists of arbitrary depth and applies function f to innermost element
;  Taken from http://stackoverflow.com/questions/5699899/scheme-map-function-for-applying-a-function-to-elements-in-a-nested-list
;  As can be seen from Github history, I had something similar but needed abstraction to avoid code repetition.
(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))

;  Gets item at position index in list
(define (get-item index list)                   
      (cond 
        [(null? list) '()]             
        [(= index 0) (car list)]
        [else (get-item (- index 1) (cdr list))]
        ))


; Add an index to a row of length 9 using custom zip function
(define (zip p q) (map list p q)) 
(define (add-column-index matrix)
  (map (lambda (x) 
         (zip x (list 0 1 2 3 4 5 6 7 8) ))
       matrix))  ;<-- gives the index


;; Removes an element from a list using standard (remove* x y) function but checks if list length is greater than 1
(define (remove-if-non-singleton lst-to-remove lst)
  (if (> (length lst) 1) 
      (remove* lst-to-remove lst)
      lst)) 





;;;; FIND SINGLETONS ;;;;

; Takes a list of lists and returns the contents of all singletons as a list 
(define (find-singleton lst)
  (acc-find-singleton lst '()))
(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  
;Finds singleton in a matrix column 
(define (find-singleton-column column-no matrix)
  (find-singleton (map (lambda (x) (get-item column-no x)) matrix)))



;;;; Sudoku Setup ;;;;

; Tranforms matrix by replacing all zeroes in a list with the (list 1-9) and all other values as a singleton list
(define (transform matrix)
  (deep-map (lambda (x) 
              (cond
                [(= x 0)(list 1 2 3 4 5 6 7 8 9) ]
                [else (list x)])) 
            matrix))


;;;; Solver Methods;;;;
; Note, I'm sure I can create a map abstraction here or use a different higher order function rather than having to map a map to iterate the 2d matrix.
; However, because filter-rows, filter-columns and filter-squares all take different params this makes it trickier. 

; Analyses a row for singleton lists and removes these values from lists in the row of length > 1 
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



; Analyses a column for singleton lists and removes these values from lists in the column of length > 1 
(define (filter-columns matrix)
   (map 
    (lambda (x) 
      (filter-columns-helper x matrix))
    (add-column-index matrix)))
  
(define (filter-columns-helper indexed-matrix matrix) 
  (map
   (lambda (x)
     (remove-if-non-singleton (find-singleton-column (cadr x) matrix) (car x)))
  indexed-matrix))
  


;;solve function to go here
; should repeatedly run filter-columns, filter-rows, filter-squares whilst checking if sudoku solved i.e every list a singleton




;;EXECUTE HERE

;(filter-columns (filter-rows (transform matrix)))





;;;; TEST START


(define grid (list 0 1 2 3 4 5 6 7 8))

(define (traverse matrix)
 (for*/list ([row grid]
             [col grid])
   ;#:when (pair? (get-item row matrix) )))
   (get-item row (list (get-item col matrix)))
   ))
  




(traverse matrix)
;;;; TEST END



;;;NOTES TO DELETE
;  once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix