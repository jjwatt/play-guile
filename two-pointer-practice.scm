(use-modules (ice-9 match))

(define (middle-element lst)
  (let loop ((fast lst)
             (slow lst))
    (match fast
      ;; Even length or odd. Fast pointer reached end.
      ((or '() (_)) slow)
      ;; Advance fast by 2, slow by 1.
      ((_ _ . fast-rest)
       (loop fast-rest
             (cdr slow)))
      (_ #f))))

(define (split-list lst)
  (let loop ((fast lst)
             (slow lst))))
