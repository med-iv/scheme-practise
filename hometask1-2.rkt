#lang scheme/base
(define (coplanar? x0 y0 z0 x1 y1 z1 x2 y2 z2)
  (define det (- (+ (* x0 y1 z2)
                    (* x1 z0 y2)
                    (* x2 y0 z1)
                    )
                 (+ (* x2 y1 z0)
                    (* x1 y0 z2)
                    (* x0 y2 z1)
                    )
                 )
    )
  (eq? det 0)
  )
    
  
  