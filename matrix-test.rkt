#lang racket

;; Author Julian Fenner ;;
;; Run by calling (solve (transform matrix))
 
(require racket/include)
(include "data.rkt")


;;;MAIN FUNCTION;;;;

;; Repeatedly calls filter-columns, filter-rows and filter-square checking if matrix solved at each iteration.
(define (solve-helper matrix)
  (let ([matrix-pre matrix]
        [matrix-post (filter-squares(filter-rows(filter-columns matrix)))])
    (cond
      [(equal? matrix-pre matrix-post) matrix]
      [else (solve-helper matrix-post)])))



;;;;SUDOKU SETUP;;;;

; Tranforms matrix by replacing all zeroes in a list with the (list 1-9) and all other values as a singleton list
(define (transform matrix)
  (deep-map (lambda (x) 
              (cond
                [(= x 0)(list 1 2 3 4 5 6 7 8 9) ]
                [else (list x)])) 
            matrix))




;;;; FIND SINGLETON FUNCTIONS ;;;;

;; Takes a list of lists and returns the contents of all singletons as a list 
(define (find-singleton lst)
  (acc-find-singleton lst '()))
(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  
;; Finds singleton element in matrix column 
(define (find-singleton-column column-no matrix)
  (find-singleton (map (lambda (x) (get-item column-no x)) matrix)))

;; Find singletons in a 3x3 grid square
(define (find-singleton-square matrix row col)
        (find-singleton (find-singleton-square-helper matrix row col)))
(define (find-singleton-square-helper matrix row col)
  (let* ([row (- row (modulo row 3))]
        [col  (- col (modulo col 3))])
    (for*/list  ([row (in-range row (+ row 3))]
                 [col (in-range col (+ col 3))])
  (get-item col (get-item row matrix))
  )))
         


;;;;SOLVER FUNCTIONS;;;;

;; Analyses a row for singleton lists and removes those values from lists in the row of length > 1 
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

;; Analyses a column for singleton lists and removes those values from lists in the column of length > 1 
(define (filter-columns matrix)
  (for*/list ([row (length matrix)])
    (let ([current-row (get-item row matrix)])
      (filter-columns-helper current-row matrix)
      )
    )
  )
(define (filter-columns-helper matrix-row matrix)
  (for*/list ([col (length matrix-row)])
    (let ([current-element (get-item col matrix-row)])
       (remove-if-non-singleton (find-singleton-column col matrix) current-element)
      )
    )
  )


;; Analyses each element's grid square for singleton lists and removes these values
(define (filter-squares matrix)
   (for*/list ([row (length matrix)])
    (let ([current-row (get-item row matrix)])
      (filter-squares-helper current-row row matrix)
      )
    )
  )
(define (filter-squares-helper matrix-row row matrix)
  (for*/list ([col (length matrix-row)])
    (let ([current-element (get-item col matrix-row)])
       (remove-if-non-singleton (find-singleton-square matrix row col) current-element)
      )
    )
  )



;;;; HELPER METHODS ;;;;

;;  Takes a list of lists of arbitrary depth and applies function f to innermost element
;;  Idea taken from http://stackoverflow.com/questions/5699899/scheme-map-function-for-applying-a-function-to-elements-in-a-nested-list.  
;;  Had similar method (see first commits) but needed better abstraction of deep-map principle
(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))

;; Gets item at position index in list
(define (get-item index list)                   
      (cond 
        [(null? list) '()]             
        [(= index 0) (car list)]
        [else (get-item (- index 1) (cdr list))]
        ))

;; Removes an element from a list using standard (remove* x y) function but checks if list length is greater than 1
(define (remove-if-non-singleton lst-to-remove lst)
  (if (> (length lst) 1) 
      (remove* lst-to-remove lst)
      lst)) 