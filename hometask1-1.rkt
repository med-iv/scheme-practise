#lang scheme/base
;2n-1!-list
(define (f n)
  (if (exact-positive-integer? n)
    (let loop ((acc 1) (counter n) (i 2) (res '()))
      (cond
        ((eq? counter 0) (reverse res))
        (else (loop (* acc i (+ i 1)) (- counter 1) (+ i 2) (cons acc res))  
          )
        )
      )
     '()
    )
  )


;2n-1!-list
(define (g n)
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
      