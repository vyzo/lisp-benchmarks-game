;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; directly ported from the go version
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

(defregister px)
(defregister py)
(defregister pz)
(defregister e)
(defregister dx)
(defregister dy)
(defregister dz)
(defregister dist)
(defregister mag)

(defstruct body (x y z vx vy vz mass))

(def pi 3.141592653589793)
(def solar-mass (fl* 4. pi pi))
(def days-per-year 365.24)

(def (offset-momentum b)
  (using (b :- body)
    (fl!= b.vx (/ (- px) solar-mass))
    (fl!= b.vy (/ (- py) solar-mass))
    (fl!= b.vz (/ (- pz) solar-mass))))

(def (system-init! system)
  (for (b system)
    (using (b :- body)
      (fl!= px (+ px (* b.vx b.mass)))
      (fl!= py (+ py (* b.vy b.mass)))
      (fl!= pz (+ pz (* b.vz b.mass)))))
  (offset-momentum (car system))
  system)

(def (energy system)
  (fl!= e 0)
  (let loop ((rest system))
    (match rest
      ([hd . rest]
       (using (b hd :- body)
         (fl!= e (+ e (* 0.5 b.mass (+ (^2 b.vx) (^2 b.vy) (^2 b.vz)))))
         (let loop-inner ((rest-inner rest))
           (match rest-inner
             ([hd . rest-inner]
              (using (b2 hd :- body)
                (fl!= dx (- b.x b2.x))
                (fl!= dy (- b.y b2.y))
                (fl!= dz (- b.z b2.z))
                (fl!= dist (sqrt (+ (^2 dx) (^2 dy) (^2 dz))))
                (fl!= e (- e (/ (* b.mass b2.mass) dist)))
                (loop-inner rest-inner)))
             (else (loop rest))))))
      (else (register-ref e)))))

(def (advance! system dt)
  (let loop ((rest system))
    (match rest
      ([hd . rest]
       (using (b hd :- body)
         (let loop-inner ((rest-inner rest))
           (match rest-inner
             ([hd . rest-inner]
              (using (b2 hd :- body)
                (fl!= dx (- b.x b2.x))
                (fl!= dy (- b.y b2.y))
                (fl!= dz (- b.z b2.z))
                (fl!= dist (+ (^2 dx) (^2 dy) (^2 dz)))
                (fl!= mag (/ dt (* dist (sqrt dist))))

                (fl!= b.vx (- b.vx (* dx b2.mass mag)))
                (fl!= b.vy (- b.vy (* dy b2.mass mag)))
                (fl!= b.vz (- b.vz (* dz b2.mass mag)))

                (fl!= b2.vx (+ b2.vx (* dx b.mass mag)))
                (fl!= b2.vy (+ b2.vy (* dy b.mass mag)))
                (fl!= b2.vz (+ b2.vz (* dz b.mass mag)))

                (loop-inner rest-inner)))
             (else
              (fl!= b.x (+ b.x (* dt b.vx)))
              (fl!= b.y (+ b.y (* dt b.vy)))
              (fl!= b.z (+ b.z (* dt b.vz)))
              (loop rest))))))
      (else (void)))))

(def jupiter
  (body (fl!  4.84143144246472090e+00)
        (fl! -1.16032004402742839e+00)
        (fl! -1.03622044471123109e-01)
        (fl! (*  1.66007664274403694e-03 days-per-year))
        (fl! (*  7.69901118419740425e-03 days-per-year))
        (fl! (* -6.90460016972063023e-05 days-per-year))
        (fl! (*  9.54791938424326609e-04 solar-mass))))

(def saturn
  (body (fl!  8.34336671824457987e+00)
        (fl!  4.12479856412430479e+00)
        (fl! -4.03523417114321381e-01)
        (fl! (* -2.76742510726862411e-03 days-per-year))
        (fl! (*  4.99852801234917238e-03 days-per-year))
        (fl! (*  2.30417297573763929e-05 days-per-year))
        (fl! (*  2.85885980666130812e-04 solar-mass))))

(def uranus
  (body (fl!  1.28943695621391310e+01)
        (fl! -1.51111514016986312e+01)
        (fl! -2.23307578892655734e-01)
        (fl! (*  2.96460137564761618e-03 days-per-year))
        (fl! (*  2.37847173959480950e-03 days-per-year))
        (fl! (* -2.96589568540237556e-05 days-per-year))
        (fl! (*  4.36624404335156298e-05 solar-mass))))

(def neptune
  (body (fl!  1.53796971148509165e+01)
        (fl! -2.59193146099879641e+01)
        (fl!  1.79258772950371181e-01)
        (fl! (*  2.68067772490389322e-03  days-per-year))
        (fl! (*  1.62824170038242295e-03  days-per-year))
        (fl! (* -9.51592254519715870e-05  days-per-year))
        (fl! (*  5.15138902046611451e-05  solar-mass))))

(def sun
  (body (fl! 0) (fl! 0) (fl! 0) (fl! 0) (fl! 0) (fl! 0) (fl! solar-mass)))

(def (main n)
  (let ((n (string->number n))
        (system (system-init! [sun jupiter saturn uranus neptune])))
    (write-output-string (format "~0,9f~n" (energy system)))
    (for (i (in-range n))
      (advance! system .01))
    (write-output-string (format "~0,9f~n" (energy system)))
    (flush-output)))
