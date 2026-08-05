(use-modules (raylib))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-16))
(use-modules (srfi srfi-11))
(use-modules (ice-9 receive))
(use-modules (system foreign)
             (rnrs bytevectors))

(define screen-width 1280)
(define screen-height 720)

(define-macro (inc! x . rest)
  `(set! ,x (+ ,x ,(if (null? rest) 1 (car rest)))))

(define-syntax with-drawing
  (syntax-rules ()
    ((_ body ...)
     (dynamic-wind
       (lambda () (BeginDrawing))
       (lambda () body ...)
       (lambda () (EndDrawing))))))

(define-syntax with-texture-mode
  (syntax-rules ()
    ((_ target body ...)
     (dynamic-wind
       (lambda () (BeginTextureMode target))
       (lambda () body ...)
       (lambda () (EndTextureMode))))))

(define-syntax with-blend-mode
  (syntax-rules ()
    ((_ target body ...)
     (dynamic-wind
       (lambda () (BeginBlendMode target))
       (lambda () body ...)
       (lambda () (EndBlendMode))))))

(define (norm value low high)
  "Normalize value to between 0.0 and 1.0."
  (/ (- value low) (- high low)))

(define (lerp low high amt)
  "Linear interpolation of amt (normalized) to low-high."
  (+ low (* amt (- high low))))

(define (mapvalue value low1 high1 low2 high2)
  "Map from one set of values to the other."
  (let ((n (norm value low1 high1)))
    (lerp low2 high2 n)))

(define pi (* 4 (atan 1.0)))

(define (deg->rad degrees)
  (* degrees (/ pi 180)))

(define %max-batch-points 4000)

;; -------------------------------------------------------------------
;; Static Raylib Records (Allocated ONCE at startup to avoid GC)
;; -------------------------------------------------------------------
(define %v1  (make-Vector2 0.0 0.0))
(define %v2  (make-Vector2 0.0 0.0))
(define %col (make-Color 0 0 0 255))

;; Mutate existing structs in-place (Zero Heap Allocations)
(define-inlinable (update-v2! v x y)
  (Vector2-set-x! v (exact->inexact x))
  (Vector2-set-y! v (exact->inexact y)))

(define-inlinable (update-color! c r g b a)
  (Color-set-r! c (inexact->exact (round (* r 255))))
  (Color-set-g! c (inexact->exact (round (* g 255))))
  (Color-set-b! c (inexact->exact (round (* b 255))))
  (Color-set-a! c (inexact->exact (round (* a 255)))))

(define* (generate-spiral-points spec #:key (turns 6.0) (steps-per-turn 120) (cx 640.0) (cy 360.0))
  (let* ((total-steps (inexact->exact (min %max-batch-points (round (* turns steps-per-turn)))))
         (d-theta     (/ (* 2.0 pi turns) total-steps))
         (points      (make-vector total-steps)))
    (do ((i 0 (+ i 1)))
        ((= i total-steps) points)
      (let* ((theta (* i d-theta))
             ;; Archimedean base math
             (r     (* 12.0 theta))
             (x     (+ cx (* r (cos theta))))
             (y     (+ cy (* r (sin theta)))))
        (vector-set! points i (cons x y))))))

(define* (draw-point-vector/lines points-vec point-count r g b a #:key (thick 2.0))
  (when (> point-count 1)
    ;; Update color once for the entire batch
    (update-color! %col r g b a)

    (let ((thick-f (exact->inexact thick)))
      (do ((i 0 (+ i 1)))
          ((= i (- point-count 1)))
        (let ((p1 (vector-ref points-vec i))
              (p2 (vector-ref points-vec (+ i 1))))

          ;; Mutate the static Vector2 instances in-place
          (update-v2! %v1 (car p1) (cdr p1))
          (update-v2! %v2 (car p2) (cdr p2))

          ;; Pass pre-allocated records directly
          (DrawLineEx %v1 %v2 thick-f %col))))))

(define (main)
  (InitWindow screen-width screen-height "Guile Raylib - Fast Batch Spirals")
  (SetTargetFPS 60)

  (let loop ((t 0.0))
    (unless (WindowShouldClose)
      ;; Pre-compute data
      (let* ((cx (/ screen-width 2.0))
             (cy (/ screen-width 2.0))
             (spiral-pts (generate-spiral-points 'archimedean
                                                 #:turns 12.0
                                                 #:steps-per-turn 150
                                                 #:cx cx
                                                 #:cy cy))
             (pt-count (vector-length spiral-pts)))
        (with-drawing
         (ClearBackground BLACK)

         (let ((r (+ 0.5 (* 0.5 (sin t))))
               (g (+ 0.5 (* 0.5 (cos (* t 0.7)))))
               (b 0.8))
           (draw-point-vector/lines spiral-pts pt-count r g b 1.0 #:thick 2.5))))
      (loop (+ t 0.016))))
  (CloseWindow))

(main)
