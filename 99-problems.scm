(use-modules (ice-9 match))

;; Problem 01
(define (my-last lst)
  (match lst
    ((x) x)
    ((_ . xs) (my-last xs))
    (_ #f)))

;; Problem 02
(define (last-two lst)
  (match lst
    ((or '() (_)) #f)
    ((x y) (list x y))
    ((_ . tail) (last-two tail))))

