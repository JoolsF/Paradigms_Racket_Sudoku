#lang racket

;; Author Julian Fenner ;;
;; Run by calling (solve (transform matrix))
 
(require racket/include)
(include "data.rkt")


;;;MAIN FUNCTION;;;;

;; Repeatedly calls filter-columns, filter-rows and filter-square checking if matrix solved at each iteration.
(define (solve matrix)
  (solve-helper (transform matrix)))

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

;; Takes a list of lists and returns the contents of all singletons as a list 
(define (find-non-singleton lst)
  (acc-find-non-singleton lst '()))
(define (acc-find-non-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(> (length (car lst)) 1)  (acc-find-non-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-non-singleton (cdr lst) resLst)]))


  
;; Finds singleton or non singleton elements in matrix column depending on what function f is defined as 
(define (find-element-column f column-no matrix)
  (f (map (lambda (x) (get-item column-no x)) matrix)))

;; Finds singleton or non singleton elements in matrix 3X3 grid depending on what function f is defined as
(define (find-element-square f matrix row col)
        (f (find-element-square-helper matrix row col)))
(define (find-element-square-helper matrix row col)
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
;;TESTING HERE!!!
(define (filter-columns-helper matrix-row matrix)
  (for*/list ([col (length matrix-row)])
    (let ([current-element (get-item col matrix-row)])
       (remove-if-non-singleton (find-element-column find-singleton col matrix) current-element)
      ;;TEST
      ;(display 'X)
      ;(display (find-element-column find-non-singleton col matrix))
      ;(display 'Y)
      (remove-if-only-option (find-element-column find-non-singleton col matrix) current-element)
      ;:TEST
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
       (remove-if-non-singleton (find-element-square find-singleton matrix row col) current-element)
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


;;TEST
(define (remove-if-only-option lst-to-remove lst-element)
  (let ([non-dupe-lst (keep-non-duplicates lst-to-remove)])
   ;(display non-dupe)
     (cond
    ;[(= (length lst-element) 1) lst-element]
     [(empty? (set-difference non-dupe-lst lst-element)) non-dupe-lst]  ;<---- BUG IS HERE
     [else lst-element])))
    
  

;; (keep-non-duplicates (list 3 3 7 6 8 8 9 9 6 4)) <----- BUG
(define (keep-non-duplicates lst)
  (keep-non-duplicates-helper lst lst '()))

(define (keep-non-duplicates-helper lst original-lst acc)
  (cond[(empty? lst) acc]
       [(= 1 (count-occurences (car lst) original-lst 0))
        (keep-non-duplicates-helper (cdr lst) original-lst (append (list(car lst)) acc))]
       [else (keep-non-duplicates-helper (cdr lst) original-lst acc)]))


(define (count-occurences character lst acc)
  (cond
    [(empty? lst) acc]
    [(eq? character (car lst)) (count-occurences character (cdr lst) (+ acc 1))]
    [else (count-occurences character (cdr lst) acc)]))
  
  










  
  ;(cond [(empty? lst) acc]
   ;     [(memq (car lst) (cdr lst)) (keep-non-duplicates-helper (cdr lst) acc)]
    ;    [else keep-non-duplicates-helper (cdr lst) (append (list(car lst)) acc)]))
 
; (> (memq 'a '(b c))
  ;   CHECK IF CAR MEMBER OF CDR IF NOT THEN ADD TO ACC

;;;;TEST AREA

;;Checks each set for elements not occuring in any other set in the same row, col, or grid square
; (remove-duplicates (append '(1 2 3) '(1 2 6)))
(define (get-row-set matrix)
   (for*/list ([row (length matrix)])
    (let ([current-row (get-item row matrix)])
      (display (remove-duplicates(flatten current-row)))
      (display 'x)
      
      
      )
    )
  )

;(set-difference  '(4 6 7 8) '(3 7 8 9 2 5 6 1)) returns 4
; This function taken from here http://stackoverflow.com/questions/11621576/list-difference-in-scheme
(define (set-difference s1 s2)
  (cond ((null? s1)
         '())
        ((not (member (car s1) s2))
         (cons (car s1) (set-difference (cdr s1) s2)))
        (else
         (set-difference (cdr s1) s2))))



