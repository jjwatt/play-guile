(use-modules (ice-9 match))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define lat?
  (match-lambda
    (() #t)
    (((? atom? head) . tail)
     (lat? tail))
    (_ #f)))

(define member?
  (match-lambda*
    (_ () #f)
    (a ((? (lambda (x) (eq? x a))) . _) #t)
    (a (_ . tail) (member? a tail))))
