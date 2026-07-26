(use-modules (srfi srfi-1))
(define (delete-adjacent-duplicates-l lst)
  (if (null? lst)
      '()
      (reverse
       (fold (lambda (elem ret)
	       (if (equal? elem (first ret))
		   ret
		   (cons elem ret)))
	     (list (first lst))
	     (cdr lst)))))

(define (delete-adjacent-duplicates-r lst)
  (fold-right (lambda (elem ret)
		(if (equal? elem (first ret))
		    ret
		    (cons elem ret)))
	      (list (last lst))
	      lst))

(define (my-fold-right kons knil lst)
  (if (null? lst)
      knil
      (kons (first lst)
	    (fold-right kons knil (cdr lst)))))

(define (my-safe-fold-right kons knil lst)
  (fold (lambda (elem acc)
	  (kons elem acc))
	knil
	(reverse lst)))

(define (my-fold kons knil lst)
  (let loop ((acc knil)
	     (curr lst))
    (if (null? curr)
	acc
	(loop (kons (car curr) acc)
	      (cdr curr)))))


(define (my-multi-fold kons knil lst1 . lists)
  (if (null? lists)
      (let loop ((acc knil)
		 (curr lst1))
	(if (null? curr)
	    acc
	    (loop (kons (car curr) acc)
		  (cdr curr))))
      (let loop ((acc knil)
		 (currs (cons lst1 lists)))
	(if (any null? currs)
	    acc
	    (let ((heads (map car currs))
		  (tails (map cdr currs)))
	      (loop (apply kons (append heads (list acc)))
		    tails))))))

(define (zip-to-alist keys values)
  (my-multi-fold (lambda (k v acc)
		   (cons (cons k v) acc))
		 '()
		 keys
		 values))


