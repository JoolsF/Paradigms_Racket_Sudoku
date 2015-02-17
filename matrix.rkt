#lang racket
(require srfi/1)

;;;NOTES TO DELETE
; (member "fall" (list "hop" "skip" "jump"))
;  once all sets of numbers are reduced to length 1, flatten each row into original format of Matrix
;  http://stackoverflow.com/questions/15871042/how-do-i-find-the-index-of-an-element-in-a-list-in-racket
; (list-index (curry equal? 2) (car (transform matrix)))
;;END OF NOTES TO DELETE


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

;; The set {1-9}
(define sudokuset(list 1 2 3 4 5 6 7 8 9))


;;replaces zeroes in list with sudokuset
(define (replace-empty x)
  (if (= x 0)
      sudokuset
      (list x))
  )


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





(define (find-singleton lst)
  (acc-find-singleton lst '()))

(define (acc-find-singleton lst resLst)
  (cond 
   [(empty? lst) resLst]
   [(= (length (car lst)) 1)  (acc-find-singleton (cdr lst) (append resLst (car lst)  ))]
   [else (acc-find-singleton (cdr lst) resLst)]))
  
 


;; Execution starts here
;; (solve matrix)
;; (transform matrix)

;(find-singleton (car(transform matrix)))



;;this needs refactoring, hard to read
(map (lambda (x)
       (cond
         [(= (length x) 1) x]
         [else (remove* (find-singleton (car(transform matrix))) x)])) 
     (car(transform matrix)))



