#lang scheme/base
(require racket/vector)

(define empty-tree #())

(define make-tree vector)


(define (create-tree cur h)
  (if (equal? h cur) empty-tree
      (let ((node (create-tree (+ cur 1) h)))
        (make-tree (expt 2 cur) node node)
        )
    )
  )

(define (task-4-2020 h)
  (create-tree 0 h)
  )
