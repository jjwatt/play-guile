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

;; Pre-allocated foreign memory buffers

;; Up to 4000 points per batch (4000 * 8 bytes = 32,000 bytes)
(define %max-batch-points 4000)

(define %points-bv (make-bytevector (* %max-batch-points 8)))
(define %points-ptr (bytevector->pointer %points-bv))

(define %color-bv (make-bytevector 4))
(define %color-ptr (bytevector->pointer %color-bv))

;; Fast mutating bytevector helpers (Zero GC)
(define-inlinable (update-color-buf! bv r g b a)
  (bytevector-u8-set! bv 0 (inexact->exact (round (* r 255))))
  (bytevector-u8-set! bv 1 (inexact->exact (round (* g 255))))
  (bytevector-u8-set! bv 2 (inexact->exact (round (* b 255))))
  (bytevector-u8-set! bv 3 (inexact->exact (round (* a 255)))))

(define-inlinable (pack-point! bv index x y)
  (let ((offset (* index 8)))
    (bytevector-ieee-single-native-set! bv offset (exact->inexact x))
    (bytevector-ieee-single-native-set! bv (+ offset 4) (exact->inexact y))))

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

(define (draw-point-vector/strip points-vec point-count r g b a)
  "Packs pre-generated points into C memory and draws in 1 call."
  (when (< 1 point-count)
    ;; Pack colors into static C buffer
    (update-color-buf! %color-bv r g b a)

    ;; Pack (x . y) values directly into raw float C array.
    (do ((i 0 (+ i 1)))
        ((= i point-count))
      (let ((p (vector-ref points-vec i)))
        (pack-point! %points-bv i (car p) (cdr p)))))
  (DrawLineStrip %points-ptr point-count %color-ptr))

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

         (let ((red (+ 0.5 (* 0.5 (sin t))))
               (green (+ 0.5 (* 0.5 (cos (* t 0.7)))))
               (blue 0.8))
           (draw-point-vector/strip spiral-pts pt-count red green blue 1.0))))
      (loop (+ t 0.016))))
  (CloseWindow))

(main)
