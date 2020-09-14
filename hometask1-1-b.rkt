#lang scheme/base
;2n-1!-list
(define (2n-1!-list n)
  (if (exact-positive-integer? n)
    (let loop ((acc 1) (counter n) (i 2))
      (cond
        ((eq? counter 0) '())
        (else (cons acc
                    (loop (* acc i (+ i 1)) (- counter 1) (+ i 2))
                )
          )
        )
      )
     '()
    )
  )
      