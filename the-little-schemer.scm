(use-modules (ice-9 match))

(define atom?
  (match-lambda
    (() #f)
    ((_ . _) #f)
    (_ #t)))

(define lat?
  (match-lambda
    (() #t)
    (((? atom? head) . tail)
     (lat? tail))
    (_ #f)))

(define member?
  (match-lambda*
    ((_ ()) #f)
    ((a ((? (lambda (x) (eq? a x)) head) . tail)) #t)
    ((a (_ . tail))
     (member? a tail))))

(define rember?
  (match-lambda*
    ((_ ()) '())
    ((a ((? (lambda (x) (eq? a x)) head) . tail)) tail)
    ((a (head . tail))
     (cons head (rember? a tail)))))

(define firsts
  (match-lambda
    (() '())
    (((first-elem . _) tail)
     (cons first-elem (firsts tail)))))

