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

(define subst
  (match-lambda*
    ((_ _ ()) '())
    ((new old ((? (lambda (x) (eq? old x)) head) . tail))
     (cons new tail))
    ((new old (head . tail))
     (cons head (subst new old tail)))))

(define subst2
  (match-lambda*
    ;; new o1 o2 lat
    ((_ _ _ ()) '())
    ((new o1 o2 ((? (lambda (x) (or (eq? o1 x) (eq? o2 x))) head) . tail))
     (cons new tail))
    ((new o1 o2 (head . tail))
     (cons head (subst2 new o1 o2 tail)))))

(define multirember
  (match-lambda*
    ((_ ()) '())
    ((a ((? (lambda (x) (eq? a x)) head) . tail))
     (multirember a tail))
    ((a (head . tail))
     (cons head (multirember a tail)))))

(define multiinsertR
  (match-lambda*
    ((_ _ ()) '())
    ((new old ((? (lambda (x) (eq? old x)) head) . tail))
     (cons old (cons new (multiinsertR new old tail))))
    ((new old (head . tail))
     (cons head (multiinsertR new old tail)))))

(define multisubst
  (match-lambda*
    ((_ _ ()) '())
    ((new old ((? (lambda (x) (eq? old x)) head) . tail))
     (cons new (multisubst new old tail)))
    ((new old (head . tail))
     (cons head (multisubst new old tail)))))

;;;; Ch. 4

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define o+
  (match-lambda*
    ((n 0) n)
    ((n m) (add1 (o+ n (sub1 m))))))

(define o-
  (match-lambda*
    ((n 0) n)
    ((n m) (sub1 (o- n (sub1 m))))))

