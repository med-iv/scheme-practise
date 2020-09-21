#lang scheme/base

;3-1
(define (fib-iter a b n res)
  (if (= n 0)
      res
      (fib-iter (+ a b) a (- n 1) (cons b res))
    )
  )

(define (list-fib-squares-b n)
  (foldl (lambda (x init)
           (cons (* x x) init)
           )
         '()
         (fib-iter 1 1 n '() )
    )
  )


;3-2
(define (process lst)
  (if (null? lst) '()
      (let ((prod-list (foldl * 1 (car lst))
              )
             )
       (filter (lambda (elem) (< prod-list (foldl + 0 elem))
                 )
         (cdr lst))
       )
    )
  )