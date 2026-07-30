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

;; Problem 07: Flatten list.
(define (my-flatten lst)
  (define (flatten lst)
    (let loop ((acc '())
               (rest lst))
      (match rest
        ('() acc)
        (((? list? head) . tail)
         (loop (loop acc head) tail))
        ((head . tail)
         (loop (cons head acc) tail))
        (_ acc))))
  (reverse (flatten lst)))

;; Problem 08: Eliminate Duplicates.
(define my-compress
  (match-lambda
    ;; Match a followed by a sub-list starting with b bound to t.
    ((a . (and (b . _) t))
     (if (equal? a b)
         (my-compress t)
         (cons a (my-compress t))))
    (smaller smaller)))

;; Alternative Problem 08: Eliminate Duplicates tail-recursive.
(define (my-compress-tail lst)
  (let loop ((acc '())
             (l lst))
    (match l
      ;; Match adjacent duplicates and bind b . _ to t.
      ((a . (and (b . _) t))
       (if (equal? a b)
           ;; Skip duplicate 'a'.
           (loop acc t)
           ;; Otherwise, keep 'a' and advance.
           (loop (cons a acc) t)))
      ((x) (reverse (cons x acc)))
      ('() (reverse acc))
      (_ #f))))

;; Problem 09: Pack consecutive duplicates.
(define (my-pack lst)
  (let loop ((acc '())
             (current '())
             (l lst))
    (match l
      ((a . (and (b . _) t))
       (if (equal? a b)
           (loop acc (cons a current) t)
           (loop (cons (cons a current) acc) '() t)))
      ((x) (reverse (cons (cons x current) acc)))
      (() '())
      (_ #f))))

;; Alternative Problem 09 with match-lambda*
(define (my-pack* lst)
  (define aux
    (match-lambda*
      ;; Emtpy input list.
      ((acc current '())
       '())
      ;; Final element
      ((acc current (x))
       (reverse (cons (cons x current) acc)))
      ;; Adjacent equal elements: add to current.
      ((acc current (a . (and (b . _) t))) (=> next)
       (if (equal? a b)
           (aux acc (cons a current) t)
           (next)))
      ;; Adjacent non-equal elements: put current onto acc
      ((acc current (a . (and (b . _) t)))
       (aux (cons (cons a current) acc) '() t))))
  (aux '() '() lst))
;; (my-pack* '(a a a b c c d e e e e f))
;; => ((a a a) (b) (c c) (d) (e e e e) (f))

;; Alternative Problem 10: RLE using Problem 9 solution
(define (my-rle-via-pack lst)
  (map (match-lambda
         ((and (x . _) sublist)
          (cons (length sublist) x)))
       (my-pack* lst)))
;; (my-rle-via-pack '(a a a b c c d))
;; =>  ((3 . a) (1 . b) (2 . c) (1 . d))


;; Problem 10: RLE
(define (my-rle lst)
  (let loop ((count 0)
             (acc '())
             (l lst))
    (match l
      ((a . (and (b . _) t))
       (if (equal? a b)
           (loop (+ count 1) acc t)
           (loop 0 (cons (cons (+ count 1) a) acc) t)))
      ((x) (reverse (cons (cons (+ count 1) x) acc)))
      (() '())
      (_ #f))))
;; (my-rle '(a a a b c c d))
;; => ((3 . a) (1 . b) (2 . c) (1 . d))

;; Problem 11: Modified RLE
(define (my-modified-rle lst)
  (let loop ((count 0)
             (acc '())
             (l lst))
    (match l
      ((a . (and (b . _) t))
       (if (equal? a b)
           (loop (+ count 1) acc t)
           (let ((entry (if (= count 0) a (cons (+ count 1) a))))
             (loop 0 (cons entry acc) t))))
      ((x)
       (let ((entry (if (= count 0) x (cons (+ count 1) x))))
         (reverse (cons entry acc))))
      (() '())
      (_ #f))))
;; (my-modified-rle '(a a a b c c d))
;; => ((3 . a) b (2 . c) d)

;; Alternative Prolbem 11: Modified RLE via my-pack*
(define (my-modified-rle-via-pack lst)
  (map (match-lambda
         ((x) x)
         ((and (x . _) sublist)
          (cons (length sublist) x)))
       (my-pack* lst)))
;; (my-modified-rle-via-pack '(a a a b c c d))
;; => ((3 . a) b (2 . c) d)
