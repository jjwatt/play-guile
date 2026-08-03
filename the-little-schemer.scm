(use-modules (ice-9 match))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define lat?
  (match-lambda
    (() #t)
    (((? atom? head) . tail)
     (lat? tail))
    (_ #f)))

(define (member? a lat)
  (match lat
    (() #f)
    ((head . tail)
     (if (eq? head a) #t
         (member? a tail)))))

(define member-match-lambda*?
  (match-lambda*
    ((_ ()) #f)
    ((a ((? (lambda (x) (eq? a x)) head) . tail)) #t)
    ((a (_ . tail))
     (member-match-lambda*? a tail))))

(define (rember? a lat)
  (match lat
    (() '())
    ((head . tail)
     (if (eq? head a)
         tail
         (cons head (rember? a tail))))))

(define rember-match-lambda*?
  (match-lambda*
    ((_ ()) '())
    ((a ((? (lambda (x) (eq? a x)) head) . tail)) tail)
    ((a (head . tail))
     (cons head (rember-match-lambda*? a tail)))))

(define firsts
  (match-lambda
    (() '())
    (((first-elem . _) tail)
     (cons first-elem (firsts tail)))))

