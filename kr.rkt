#lang scheme/base

;1

(define (fun1 lst)
  (define (func elem maxims)
    (cond ((> elem (car maxims)) (list elem (car maxims)))
          ((> (car maxims) elem (cadr maxims)) (list (cadr maxims) elem))
          (else maxims)
          )
  )
  (define ans (cadr (foldl func '(-inf.0 -inf.0) lst)))
  (if (= ans -inf.0) '()
      ans)
  )

;2
(define (is-prime? n)
  (define (prime n s)
    (cond ((= s n) #t)
          ((= 0 (remainder n s)) #f)
           (else (prime n (+ s 1)))
           )
    )
  (prime n 2)
  )

(define (fun2a n)
  (let loop ((s 2))
    (cond ((> s n) '())
          ((and (= 0 (remainder n s)) (not (is-prime? s))) (cons s (loop (+ s 1))))
          (else (loop (+ s 1)))
          )
    )
  )

(define (fun2b n)
  (reverse (let loop ((s 2)
                      (res '()))
             (cond ((> s n) res)
                   ((and (= 0 (remainder n s)) (not (is-prime? s))) (loop (+ s 1) (cons s res)))
                   (else (loop (+ s 1) res))
                   )
             )
    )
  )

;3

(require math/number-theory)

(define (fun3 n)
  (define (func s counter res)
    (cond ((> counter n) res)
          ((prime? s) (func (+ s 1) counter res))
          (else (func (+ s 1) (+ counter 1) (* res s)))
          )
    )
  (func 1 0 1)
  )

;4
(require racket/vector)

(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (tree-empty? t) (equal? t #()))

(define (fun4 tree r1 r2)
  (define (desc t d r1 r2)
    (cond ((tree-empty? t) 0)
          ((<= r1 d r2) (+ (if (> (tree-data t) 0) 1 0)
                            (desc (tree-left t) (+ d 1) r1 r2) (desc (tree-right t) (+ d 1) r1 r2)))
          ((> d r2) 0)
          (else (+ (desc (tree-left t) (+ d 1) r1 r2) (desc (tree-right t) (+ d 1) r1 r2)))
          )
    )
  (if (< r1 r2) (desc tree 0 r1 r2)
      (desc tree 0 r2 r1)
      )
  )

;5

(define (fun5-cps tree r1 r2 cc)
  (define (desc t d r1 r2 cc)
    (cond ((or (tree-empty? t) (> d r2)) (cc 0))
          ((<= r1 d r2) (if (> (tree-data t) 0)
                            (desc (tree-left t) (+ d 1) r1 r2
                                  (lambda (y) (desc (tree-right t) (+ d 1) r1 r2
                                                    (lambda (z) (cc (+ 1 y z))))))
                            (desc (tree-left t) (+ d 1) r1 r2
                                  (lambda (y) (desc (tree-right t) (+ d 1) r1 r2
                                                    (lambda (z) (cc (+ y z))))))
                            ))      
          (else (desc (tree-left t) (+ d 1) r1 r2
                                  (lambda (y) (desc (tree-right t) (+ d 1) r1 r2
                                                    (lambda (z) (cc (+ y z)))
                                                    )
                                    )
                                  )
                )
          )
    )
  (if (< r1 r2) (desc tree 0 r1 r2 cc)
      (desc tree 0 r2 r1 cc)
      )
  )