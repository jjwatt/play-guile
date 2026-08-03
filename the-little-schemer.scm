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

(define insertR
  (match-lambda*
    ((_ _ ()) '())
    ((new old ((? (lambda (x) (eq? old x)) head) . tail))
     (cons head (cons new tail)))
    ((new old (head . tail))
     (cons head (insertR new old tail)))))

(define insertL
  (match-lambda*
    ((_ _ ()) '())
    ((new old ((? (lambda (x) (eq? old x)) head) . tail))
     (cons new (cons head tail)))
    ((new old (head . tail))
     (cons head (insertL new old tail)))))
