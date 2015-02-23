;;my-helper-functions


;; deep-map taken from http://stackoverflow.com/questions/5699899/scheme-map-function-for-applying-a-function-to-elements-in-a-nested-list
;; as can be seen from github history, I'd been using a process of nested maps to iterate a 2d list but needed an efficient way of abstracting this
;; to avoid code repetition and to increase readability
(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))