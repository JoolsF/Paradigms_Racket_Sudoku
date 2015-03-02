


;; HELPER FUNCTIONS - To move other module
;; deep-map taken from http://stackoverflow.com/questions/5699899/scheme-map-function-for-applying-a-function-to-elements-in-a-nested-list
;; as can be seen from github history, I'd been using a process of nested maps to iterate a 2d list but needed an efficient way of abstracting this
;; to avoid code repetition and to increase readability
(define (deep-map-innermost f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))

; Helper method to get nth item of list
(define (get-item number list)                   
      (cond 
        [(null? list) '()]             
        [(= number 0) (car list)]
        [else (get-item (- number 1) (cdr list))]
        )
  )


; TO DO - refactor 
;Applies function to innermost lists of a matrix
;(define (deep-map-innermost-list f l)
 ; (let deep ((x l)) 
  ;  (cond [(null? x) x]
   ;       [(and (pair? x) (number? (car x)))
    ;       (f x)]
     ;     (else (map deep x)))))


(define (zip p q) (map list p q)) 





;; END OF HELPER FUNCTIONS






;ADD COLUMN INDEX TO MATRIX
(define (add-column-index matrix)
  (map (lambda (x) 
         (zip x (list 0 1 2 3 4 5 6 7 8) ))
       matrix))  ;<-- gives the index