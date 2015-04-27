#lang racket

;; Author Julian Fenner ;;
;; Run by calling (solve matrix))
 
(require racket/include)
(include "data.rkt")

;;;THIS IS NOT THE MAIN PROGRAM, THIS IS TO SHOW WORKING OUT;;;
;;; PLEASE SEE matrix.rkt AND REFER TO GITHUB README;;;;


;;;MAIN FUNCTION;;;;

;; Repeatedly calls filter-columns, filter-rows and filter-square checking if matrix solved at each iteration.
(define (solve matrix)
  (solve-helper (transform matrix)))

(define (solve-helper matrix)
  (let ([matrix-pre matrix]
        [matrix-post (jtest
                      (filter-squares
                       (filter-rows
                        (filter-columns matrix))))])
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
         (filter-row x)) 
       matrix))
(define (filter-row matrix)
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
      (remove-if-non-singleton (find-element-column find-singleton col matrix) current-element)
      )
    ))



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








;;;;;NOT USED WORK IN PROGRESS BELOW;;;;;
; Ser github "readme"
; These functions are for the second part of the algorithm
; - Find a number in a set that does not occur in any other set in the same row (or column, or box).
; - Reduce that set to a singleton containing that one number.
; However, there is a bug when integrating them into main program so not being used currently. Running (solve matrix) on this version gives an error on row 3
; The solution below is also verbose and in need of streamlining and abstracting


; (remove-if-non-singleton (find-element-column find-singleton col matrix) current-element)


;;BUG BUT NOT A MILLION MILES OFF
(define (jtest matrix)
   (for*/list ([row (length matrix)])
    (let ([current-row (get-item row matrix)])
     (for*/list ([col (length current-row)])
       (let* ([currentsquare (keep-non-duplicates(find-element-square find-non-singleton matrix row col))]
             [currentcolumn  (keep-non-duplicates(find-element-column find-non-singleton col matrix))]
             [currentrow (keep-non-duplicates (find-non-singleton current-row))]
             [currentelement (get-item col current-row)]
             ;;INTEGRATE FIND SINGLETONS THESE SHOULD BE REMOVED FROM RESULT
             
             [current-sq-singleton (find-element-square find-singleton matrix row col)]
             [current-cl-singleton (find-element-column find-singleton col matrix)]
             [current-el-singleton (filter-row current-row)]
             [singletons (remove-duplicates(flatten(append current-sq-singleton current-cl-singleton current-el-singleton)))]             
                         ;;END
             [possiblevalues (remove-duplicates(append currentsquare currentcolumn currentrow))] ;<---- BUG IS HERE
             [result (remove-duplicates(keep-duplicates(append possiblevalues currentelement)))] ;<----- OR HERE
            
             )
         
        ; (display 'xxxx)
        ; (display currentelement)
        ; (display possiblevalues)
        ; (display result)
        ; (display singletons)
        ; (display (singleton-in-result singletons result))
         (cond
           ;[(eq? #t (singleton-in-result singletons result)) currentelement] <--- THIS CHECK NEEDS TO BE INTEGRATED
           [(= 1 (length result)) result]
           [else currentelement]))))))

(define (singleton-in-result singletons result)
  (let ([check (keep-duplicates(append singletons result))])
    (cond
      [(empty? check) #f]
      [else #t])))
 
;;Takes a list and returns all those elements in the list that are not duplicates
(define (keep-non-duplicates lst)
  (keep-non-duplicates-helper lst lst '()))
(define (keep-non-duplicates-helper lst original-lst acc)
  (cond[(empty? lst) acc]
       [(= 1 (count-occurences (car lst) original-lst 0))
        (keep-non-duplicates-helper (cdr lst) original-lst (append (list(car lst)) acc))]
       [else (keep-non-duplicates-helper (cdr lst) original-lst acc)]))


;;KEEP DUPLICATES
(define (keep-duplicates lst)
  (keep-duplicates-helper lst lst '()))
(define (keep-duplicates-helper lst original-lst acc)
  (cond[(empty? lst) acc]
       [(>  (count-occurences (car lst) original-lst 0) 1)
        (keep-duplicates-helper (cdr lst) original-lst (append (list(car lst)) acc))]
       [else (keep-duplicates-helper (cdr lst) original-lst acc)]))

;Count occurrences of a given character in a list
(define (count-occurences character lst acc)
  (cond
    [(empty? lst) acc]
    [(eq? character (car lst)) (count-occurences character (cdr lst) (+ acc 1))]
    [else (count-occurences character (cdr lst) acc)]))
  
  

(define (remove-duplicates lst)
  (remove-duplicates-helper lst '()))
(define (remove-duplicates-helper lst acc)
  (cond
    [(empty? lst) acc]
     [(eq? #f (memq (car lst) acc)) (remove-duplicates-helper (cdr lst) (append (list (car lst)) acc))]
     [else (remove-duplicates-helper (cdr lst) acc)]))

;bug exists here when jtest applied
(define bug '(((7 8 9) (2)   (5) (3 7 8) (3 8) (1) (9) (3 7) (4))
              ((1)     (3 7) (4) (2) (5) (9) (8) (3 7) (6))
              ((7 8 9) (3 7) (6) (3 7 8) (3 8) (4) (2) (1) (5))
              ((4)     (5) (9) (1) (6) (6) (3) (2) (7))
              ((6)     (1) (3) (4) (2) (7) (7) (5) (9))
              ((2)     (8) (7) (1) (3 9) (5) (4) (6) (1))
              ((3)     (9) (1) (5) (4) (2) (6) (7) (8))
              ((5)     (6) (2) (9) (7) (8) (1) (4) (3))
              ((7)     (4) (8) (6) (1) (3) (5) (9) (2))))
       
;;this is the solution now when jtest applied to solve algorithm.  Errors appear be on first three rows.  Perhaps grid function is at fault?
(define bug2 '(
  ((9) (2) (5) (7) (8) (1) (9) (3) (4))
  ((1) (7) (4) (2) (5) (9) (8) (3) (6))
  ((9) (3) (6) (3) (3) (4) (2) (1) (5))
  ((4) (5) (9) (1) (6) (6) (3) (2) (7))
  ((6) (1) (3) (4) (2) (7) (7) (5) (9))
  ((2) (8) (7) (1) (9) (5) (4) (6) (1))
  ((3) (9) (1) (5) (4) (2) (6) (7) (8))
  ((5) (6) (2) (9) (7) (8) (1) (4) (3))
  ((7) (4) (8) (6) (1) (3) (5) (9) (2))))
> 