(use-modules (srfi srfi-9))
(use-modules (srfi srfi-19))


(use-modules (ice-9 match))

;; Extract values from lists.
(match '(1 2 3)
  ((x y z) (+ x y z)))

;; Destructure head and tail (pairs).
(match '(1 2 3 4)
  ((head . tail) (list head tail)))
;; =>  (1 (2 3 4))

;; ... captures everything as a list.
(match '(1 2 3)
  ((numbers ...)
   (format #f "Here are some numbers: ~a" numbers))
  (_ "Did not match!"))
;; =>  "Here are some numbers: (1 2 3)"

;; Match vectors vs. lists.
(match #(1 2 3)
  ((x y z)
   "It's a list!")
  (#(x y z)
   (format #f "y is ~a" y))
  (_ "Did not match!"))
;; =>  "y is 2"

;; Structural unzipping.
(match '(("a" 1) ("b" 2) ("c" 3))
  (((keys values) ...)
   (format #f "Keys: ~a | Values: ~a" keys values)))
;; => "Keys: (a b c) | Values: (1 2 3)"

(let loop ((items '(1 2 3 4 5))
           (strings '()))
  (match items
    ((item . rest)
     (loop rest
           (cons (format #f "Item ~a" item)
                 strings)))
    (() (reverse strings))))
;; => ("Item 1" "Item 2" "Item 3" "Item 4" "Item 5")

;; Compare with
(let loop ((items '(1 2 3 4 5))
           (strings '()))
  (if (pair? items)
      (loop (cdr items)
            (cons (format #f "Item ~a" (car items))
                  strings))
      (reverse strings)))
;; => ("Item 1" "Item 2" "Item 3" "Item 4" "Item 5")

(define-record-type person
  (make-person name age hometown)
  person?
  (name person-name)
  (age person-age)
  (hometown person-hometown))

(match (make-person "David" 132 "Mars")
  (($ person name age hometown)
   (format #f "~a is ~a and from ~a." name age hometown))
  (_ "Not a 'person' record!"))

(match (make-person "David" 132 "Mars")
  ((= person-age 50)
   "The person is 50 years old.")
  (_ "Not 50 years old!"))

(match (make-person "David" 50 "Mars")
  ((= person-age 50)
   "The person is 50 years old.")
  (_ "Not 50 years old!"))

(match (make-person "David" 50 "Mars")
  ((? person? person)
   (format #f "The person is ~a years old." (person-age person)))
  (_ "Not 50 years old!"))

(define match-numbers
  (match-lambda
    ((and (1 y z)
          (x y z))
     (format #f "y is ~a" y))
    (_ "Did not match!")))

(match-numbers '(1 2 3))
(match-numbers '(3 2 1))

(define match-numbers*
  (match-lambda*
    ((and (1 y z)
          (x y z))
     (format #f "y is ~a" y))
    (_ "Did not match!")))

(match-numbers* 1 2 3)
(match-numbers* 3 2 1)

(define my-values '(1 (1 2) 3))

(match-let (((x y z) my-values))
  (format #f "y is ~a" y))

(match-let (((x (a b) z) my-values))
  (format #f "b is ~a" b))

(match-let* (((x y z) my-values)
             ((a b) y))
  (format #f "b is ~a" b))

(define (sum-key-values pairs)
  (match-let loop ((((key . val) . rest) pairs)
                   (acc 0))
    (let ((new-acc (+ acc val)))
      (if (null? rest)
          new-acc
          (loop rest new-acc)))))

(sum-key-values '(("a" . 10) ("b" . 20) ("c" . 30)))

(define factorial
  (match-lambda
    ((or 0 1) 1)
    ((? number? n) (* n (factorial (- n 1))))
    (_ "Invalid input. It should be an integer.")))

(define itemize
  (match-lambda
    ((? list? lst) (reverse (itemizer lst '())))
    (_ "Invalid input. It should be a list.")))

(define itemizer
  (match-lambda*
    ([() acc] acc)
    ([(head . tail) acc]
     (itemizer tail (cons (format #f "Item ~a" head) acc)))))

(itemize '(1 2 3 4 5))

;;;; Advanced Matching

;; Guards (=>)
(define (classify-number val)
  (match val
    ((? number? n) (=> next)
     (if (even? n)
         (format #f "~a is even" n)
         (next)))
    ((? number? n)
     (format #f "~a is odd" n))
    (_ "Not a number")))

(classify-number 4)
;; => "4 is even"
(classify-number 3)
;; => "3 is odd"

;; Segment matching (... and ___)
(define (parse-command cmd)
  (match cmd
    ;; Matches "connect <host> port <p1> <p2> ..."
    (('connect host 'port ports ...)
     (format #f "Connecting to ~a on ports ~a" host ports))
    ;; Matches nested structural list paris.
    (((keys values) ...)
     (format #f "Keys: ~a, Values: ~a" keys values))
    (_ "Unknown command")))

(parse-command '(connect "localhost" port 80 443 8080))
;; => "Connecting to localhost on ports (80 443 8080)"

(parse-command '(("a" 1) ("b" 2) ("c" 3)))
;; => "Keys: (a b c), Values: (1 2 3)"

;; ... doesn't have to be at the beginning or the end.
(define (parse-header-and-footer lst)
  (match lst
    ((head middle ... tail)
     (format #f "Head: ~a | Mid: ~a | Tail: ~a" head middle tail))
    (_ "List too short")))

(parse-header-and-footer '(START a b c d END))
;; => "Head: START | Mid: (a b c d) | Tail: END"
(parse-header-and-footer '(START END))
;; =>  "Head: START | Mid: () | Tail: END"


;; ___ is an alias for ... so you can use it in macros.
(define-syntax-rule (make-list-checker prefix)
  (lambda (lst)
    (match lst
      ((prefix items ___) (format #f "Matched prefix ~a with items ~a" prefix items))
      (_ "No match"))))
(define check-admin (make-list-checker 'admin))
(check-admin '(admin 1 2 3 4))
;; => "Matched prefix admin with items (1 2 3 4)"


;; Using '=': Applies a function to incoming data and matches
;; the result against the sub-pattern.
;; ? is a predicate guard. It has to match the predicate.
(define (greeting-by-time user-time)
  (match user-time
    ((= date-hour (and h (? (lambda (hr) (< hr 12)))))
     (format #f "Good morning! (Hour ~a)" h))
    ((= date-hour h)
     (format #f "Hello! (Hour ~a)" h))))

(greeting-by-time (current-date))
;; => "Hello! (Hour 16)"

;; Record Matching ($)
(define-record-type <employee>
  (make-employee id name salary department)
  employee?
  (id employee-id)
  (name employee-name)
  (salary employee-salary)
  (department employee-department))

(define (tech-bonus emp)
  (match emp
    ;; Match only employees in Engineering with salary < 100000
    (($ <employee> id name (? (lambda (s) (< s 100000)) sal) "Engineering")
     (format #f "Grant bonus to ~a (ID: ~a)" name id))
    (($ <employee> _ name _ dept)
     (format #f "No bonus for ~a in ~a" name dept))))

(tech-bonus (make-employee 101 "Alice" 85000 "Engineering"))
;; => "Grant bonus to Alice (ID: 101)"

