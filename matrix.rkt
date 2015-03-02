#lang racket

(require racket/include)
(include "data.rkt")
(include "matrix-helper.rkt")


;; FIND SINGLETONS AND GATHERS THEM INTO SINGLE LIST
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


;;Finds singleton in column
(define (find-singleton-column item-no matrix)
  (find-singleton (map (lambda (x) (get-item item-no x)) matrix)))





;; SETS UP THE SUDOKU  
;;Tranforms matrix by replacing all zeroes in a list with the (list 1-9) and all other values as a singleton list
(define (transform matrix)
  (deep-map-innermost (lambda (x) 
              (cond
                [(= x 0)(list 1 2 3 4 5 6 7 8 9) ]
                [else (list x)])) 
            matrix))

;;SOLVES THE SUDOKU
(define (solve matrix)
  (map (lambda (x) 
         (remove-singleton-from-sets x)) 
       matrix))



;;TEST AREA


(define (remove-if-non-singleton lst-to-remove lst)
  (if (> (length lst) 1) 
      (remove* lst-to-remove lst)
      lst)) 



(define (remove-col-dupe matrix)
  (map (lambda (indexed-matrix) 
         (remove-col-dupe-helper indexed-matrix matrix))
       (add-column-index matrix)))



(define (remove-col-dupe-helper indexed-matrix matrix)
  ; (print (append indexed-matrix (list "XX"))))
  ;(map (lambda (x) (print (car x)))
  (map (lambda (x)
        
        (print (remove-if-non-singleton (find-singleton-column (cadr x) matrix) (car x)))
         (car x))
       indexed-matrix))
  



  
  (remove-col-dupe (solve(transform matrix)))
  
  

;(define (remove-col-dupe-helper indexed-matrix matrix)
 ; (let deep ((x matrix)) 
  ;  (cond [(null? x) x]
   ;       [(and (pair? x) (number? (cadr x)))
    ;       (print (cadr x))]
           ;(remove-if-non-singleton (find-singleton-column (cadr x) matrix) (car x))]
     ;     (else (map deep x)))))






;(define (remove-col-dupe-helper indexed-matrix matrix)
 ; (deep-map-innermost-list (lambda (x) 
                         ; (remove-if-non-singleton (find-singleton-column 0 matrix) x)
   ;     (print (append x (list "XX"))))
    ;                       indexed-matrix))



;(define (deep-map-innermost-list f l)
 ; (let deep ((x l)) 
  ;  (cond [(null? x) x]
   ;       [(and (pair? x) (number? (cadr x)))
    ;       (f x)]
     ;     (else (map deep x)))))

       


;;;;TEST AREA

;;START HERE NEED TO APPLY SOLVE METHODS TO DIFFERENT LIST STRUCTURE
;; these are the inner element after ---> (add-column-index (transform matrix))
 (define TESTLIST (list (list 1 2 3 4 5 6 7 8 9) 0))
;; base case is (pair? (cadr TESTLIST)) which is the index


(define JOOLS (list(list(list 1 2 3 4 5 6 7 8 9) 0) (list (list 2) 1) (list(list 5) 2) (list( list 1 2 3 4 5 6 7 8 9) 3) (list (list 1 2 3 4 5 6 7 8 9) 4) (list (list 1) 5) (list ( list 1 2 3 4 5 6 7 8 9) 6) (list (list 1 2 3 4 5 6 7 8 9) 7) (list (list 1 2 3 4 5 6 7 8 9) 8)))
;;;;END OF TEST AREA




;(solve(add-column-index (transform matrix)))

(define TESTY (add-column-index (solve(transform matrix))))

;(transform matrix)
;(solve (transform matrix))
;(add-column-index (solve (transform matrix)))

;(remove-col-dupe (solve(transform matrix)))





;;;NOTES TO DELETE
; (member "fall" (list "hop" "skip" "jump"))
;  once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix
;  http://stackoverflow.com/questions/15871042/how-do-i-find-the-index-of-an-element-in-a-list-in-racket
; (list-index (curry equal? 2) (car (transform matrix)))
;;END OF NOTES TO DELETE