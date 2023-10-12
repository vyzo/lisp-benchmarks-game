;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from the go version and optimized
(import :std/error
        :std/sugar
        :std/contract
        :std/iter
        :std/format
        :std/text/utf8
        :std/os/fdio
        :gerbil/gambit)
(export main)
(declare
  (not safe)
  (fixnum))
(include "io.ss")
(include "flonum.ss")

;; floating point registers
(defregister px)
(defregister py)
(defregister pz)
(defregister e)
(defregister dx)
(defregister dy)
(defregister dz)
(defregister dist)
(defregister mag)
(defregister x1)
(defregister y1)
(defregister z1)
(defregister vx1)
(defregister vy1)
(defregister vz1)
(defregister m1)
(defregister m2)
(defregister dt .01)

;; the celestial body
(def (body x y z vx vy vz mass)
  (f64vector x y z vx vy vz mass))

(def pi 3.141592653589793)
(def solar-mass (fl* 4. pi pi))
(def days-per-year 365.24)

(def (offset-momentum b)
  (fl!= (@ b 3) (/ (- px) solar-mass))
  (fl!= (@ b 4) (/ (- py) solar-mass))
  (fl!= (@ b 5) (/ (- pz) solar-mass)))

(def (system-init! system)
  (for (b system)
    (fl!= px (+ px (* (@ b 3) (@ b 6))))
    (fl!= py (+ py (* (@ b 4) (@ b 6))))
    (fl!= pz (+ pz (* (@ b 5) (@ b 6)))))
  (offset-momentum (car system))
  system)

(def (energy system)
  (fl!= e 0)
  (let loop ((rest system))
    (match rest
      ([b . rest]
       (fl!= e (+ e (* 0.5 (@ b 6) (+ (^2 (@ b 3)) (^2 (@ b 4)) (^2 (@ b 5))))))
       (let loop-inner ((rest-inner rest))
         (match rest-inner
           ([b2 . rest-inner]
            (fl!= dx (- (@ b 0) (@ b2 0)))
            (fl!= dy (- (@ b 1) (@ b2 1)))
            (fl!= dz (- (@ b 2) (@ b2 2)))
            (fl!= dist (sqrt (+ (^2 dx) (^2 dy) (^2 dz))))
            (fl!= e (- e (/ (* (@ b 6) (@ b2 6)) dist)))
            (loop-inner rest-inner))
           (else (loop rest)))))
      (else (register-ref e)))))

(def (advance! system)
  (let loop ((rest system))
    (match rest
      ([b . rest]
       ;; load the registers
       (fl!= x1 (@ b 0))
       (fl!= y1 (@ b 1))
       (fl!= z1 (@ b 2))
       (fl!= vx1 (@ b 3))
       (fl!= vy1 (@ b 4))
       (fl!= vz1 (@ b 5))
       (fl!= m1 (@ b 6))
       (let loop-inner ((rest-inner rest))
         (match rest-inner
           ([b2 . rest-inner]
            (fl!= m2 (@ b2 6))
            (fl!= dx (- x1 (@ b2 0)))
            (fl!= dy (- y1 (@ b2 1)))
            (fl!= dz (- z1 (@ b2 2)))
            (fl!= dist (+ (^2 dx) (^2 dy) (^2 dz)))
            (fl!= mag (/ dt (* dist (sqrt dist))))

            (fl!= vx1 (- vx1 (* dx m2 mag)))
            (fl!= vy1 (- vy1 (* dy m2 mag)))
            (fl!= vz1 (- vz1 (* dz m2 mag)))

            (fl!= (@ b2 3) (+ (@ b2 3) (* dx m1 mag)))
            (fl!= (@ b2 4) (+ (@ b2 4) (* dy m1 mag)))
            (fl!= (@ b2 5) (+ (@ b2 5) (* dz m1 mag)))

            (loop-inner rest-inner))
           (else
            ;; store the registers back to the object
            (fl!= (@ b 0) (+ x1 (* dt vx1)))
            (fl!= (@ b 1) (+ y1 (* dt vy1)))
            (fl!= (@ b 2) (+ z1 (* dt vz1)))
            (unless (null? rest)
              (fl!= (@ b 3) vx1)
              (fl!= (@ b 4) vy1)
              (fl!= (@ b 5) vz1)
              (loop rest))))))
      (else (void)))))

(def jupiter
  (body 4.84143144246472090e+00
        -1.16032004402742839e+00
        -1.03622044471123109e-01
        (fl*  1.66007664274403694e-03 days-per-year)
        (fl*  7.69901118419740425e-03 days-per-year)
        (fl* -6.90460016972063023e-05 days-per-year)
        (fl*  9.54791938424326609e-04 solar-mass)))

(def saturn
  (body   8.34336671824457987e+00
          4.12479856412430479e+00
          -4.03523417114321381e-01
          (fl* -2.76742510726862411e-03 days-per-year)
          (fl*  4.99852801234917238e-03 days-per-year)
          (fl*  2.30417297573763929e-05 days-per-year)
          (fl*  2.85885980666130812e-04 solar-mass)))

(def uranus
  (body   1.28943695621391310e+01
          -1.51111514016986312e+01
          -2.23307578892655734e-01
          (fl*  2.96460137564761618e-03 days-per-year)
          (fl*  2.37847173959480950e-03 days-per-year)
          (fl* -2.96589568540237556e-05 days-per-year)
          (fl*  4.36624404335156298e-05 solar-mass)))

(def neptune
  (body   1.53796971148509165e+01
          -2.59193146099879641e+01
          1.79258772950371181e-01
          (fl*  2.68067772490389322e-03  days-per-year)
          (fl*  1.62824170038242295e-03  days-per-year)
          (fl* -9.51592254519715870e-05  days-per-year)
          (fl*  5.15138902046611451e-05  solar-mass)))

(def sun
  (body 0.0 0.0 0.0 0.0 0.0 0.0 solar-mass))

(def (main n)
  (let ((n (string->number n))
        (system (system-init! [sun jupiter saturn uranus neptune])))
    (write-output-string (format "~0,9f~n" (energy system)))
    (for (i (in-range n))
      (advance! system))
    (write-output-string (format "~0,9f~n" (energy system)))
    (flush-output)))
