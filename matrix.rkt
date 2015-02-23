#lang racket
(require racket/include)
(include "data.rkt")


;;;NOTES TO DELETE
; (member "fall" (list "hop" "skip" "jump"))
;  once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix
;  http://stackoverflow.com/questions/15871042/how-do-i-find-the-index-of-an-element-in-a-list-in-racket
; (list-index (curry equal? 2) (car (transform matrix)))
;;END OF NOTES TO DELETE


;; The set {1-9}
(define sudokuset(list 1 2 3 4 5 6 7 8 9))


;;replaces zeroes in list with sudokuset
(define (replace-empty x)
  (if (= x 0)
      sudokuset
      (list x))
  )

; TO DO - Refactor these functions, see http://stackoverflow.com/questions/5699899/scheme-map-function-for-applying-a-function-to-elements-in-a-nested-list
;; For mapping each matrix row's element to function f
(define (map-matrix f lst)
  (map (lambda (x) 
         (f x)) 
       lst))

;;tranforms matrix using replace-empty
(define (transform matrix)
  (map (lambda (x) 
         (map-matrix replace-empty x)) 
       matrix))



;;takes a list of lists and returns  lists with a single element
(define (find-singleton lst)
  (acc-find-singleton lst '()))

(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  
(define (remove-singleton-matches-rows matrix)
  (map (lambda (x)
         (cond
           [(= (length x) 1) x]
           [else (remove* (find-singleton matrix) x)]))
         matrix))




(define (solve matrix)
  (map (lambda (x) 
         (remove-singleton-matches-rows x)) 
       matrix))


;; Execution starts here
;; (solve matrix)
;; (transform matrix)




;(solve (transform matrix))

