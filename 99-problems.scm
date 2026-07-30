(use-modules (ice-9 match))

;; Problem 01: Last of a list.
(define (my-last lst)
  (match lst
    ((x) x)
    ((_ . xs) (my-last xs))
    (_ #f)))

;; Problem 02: Last two of a list.
(define (last-two lst)
  (match lst
    ((or '() (_)) #f)
    ((x y) (list x y))
    ((_ . tail) (last-two tail))))

;; Problem 03: Get element at k.
(define (my-at k lst)
  (match lst
    ((x . xs) (if (= k 0) x (my-at (- k 1) xs)))
    (_ #f)))

;; Alternative Problem 03 with ?
(define (at-guard k lst)
  (match lst
    ;; k is 0 and the list has at least one element.
    (((? (lambda (_) (= k 0)) x) . _) x)
    ;; k > 0 and there's a head and tail.
    ((_ . xs) (at-guard (- k 1) xs))
    ;; Empty list or invalid k.
    (_ #f)))

;; Alternative Problem 03 with match-lambda*
(define at-match-lambda*
  (match-lambda*
    ((0 (x . _)) x)
    ((k (_ . xs)) (at-match-lambda* (- k 1) xs))
    (_ #f)))

;; Problem 04: Length of a list.
(define (my-length lst)
  (define (aux n lst)
    (match lst
      (() n)
      ((_ . xs) (aux (+ n 1) xs))
      (_ #f)))
  (aux 0 lst))

;; Alternative Problem 04 with match-lambda*
(define (my-length-match-lambda* lst)
  (define aux
    (match-lambda*
      ((n ()) n)
      ((n (_ . xs)) (aux (+ n 1) xs))
      (_ #f)))
  (aux 0 lst))

;; Alternative Problem 04 with named-let
(define (my-length-let lst)
  (let loop ((n 0)
             (l lst))
    (match l
      (() n)
      ((_ . t) (loop (+ n 1) t))
      (_ #f))))

;; Problem 05: Reverse a List.
(define (my-rev lst)
  (let loop ((acc '())
             (l lst))
    (match l
      (() acc)
      ((x . xs) (loop (cons x acc) xs))
      (_ #f))))

;; Alternative Problem 05 with letrec and match-lambda.
(define my-rev-match-lambda*
  (letrec ((aux (match-lambda*
                  ((acc ()) acc)
                  ((acc (x . xs)) (aux (cons x acc) xs))
                  (_ #f))))
    (lambda (lst)
      (aux '() lst))))

;; Problem 06: Palindrome
(define (palindrome? lst)
  (equal? lst (reverse lst)))

;; Alternative Problem 06 with matching.
(define (palindrome-match? lst)
  (match lst
    ((or '() (_)) #t)
    ((x mid ... y)
     (and (equal? x y)
          (palindrome-match? mid)))
    (_ #f)))

;; Alternative Problem 06 Fast Pointer/Half-Reversal.
(define (palindrome-fast? lst)
  (let loop ((fast lst)
             (slow lst)
             (rev  '()))
    (match fast
      ;; Fast pointer reached end (even length): compare reversed first half.
      ('() (equal? rev slow))
      ;; Fast pointer has one element left (odd length): skip middle element.
      ((_) (equal? rev (cdr slow)))
      ;; Advance fast by 2, slow by 1, pushing onto rev.
      ((_ _ . fast-rest)
       (loop fast-rest
             (cdr slow)
             (cons (car slow) rev)))
      (_ #f))))
