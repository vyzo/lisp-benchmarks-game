#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/sort
        :std/misc/ports)

(def games
  '("fannkuchredux"
    "nbody"
    "spectralnorm"
    "mandelbrot"
    "pidigits"
    "fasta"
    "binarytrees"))

(def players
  '("racket" "go" "gcc" "gerbil"))

(def (generate-table output)
  (parameterize ((current-output-port output))
    (displayln "<table>")
    (displayln "<tr>")
    (displayln "<th>Benchmark</th>")
    (for (p players)
      (printf "<th align=\"center\">⋄⋄⋄</th>")
      (printf "<th align=\"center\">~a</th>~n" p))
    (displayln "</tr>")
    (for (g games)
      (generate-row g))
    (displayln "</table>")))

(def (generate-row g)
  (let* ((output (read-file-u8vector (string-append "programs/" g "/output")))
         (player-output
          (for/collect (p players)
            (let (p-output (string-append "programs/" g "/" p ".output"))
               (if (file-exists? p-output)
                 (let (p-output (read-file-u8vector p-output))
                   (if (equal? output p-output)
                     'CORRECT
                     '|INCORRECT OUTPUT|))
                 '|NO PROGRAM|))))
         (player-results
          (for/collect ((p players)
                        (o player-output))
            (case o
              ((CORRECT)
               (let (lines (read-file-lines (string-append "programs/" g "/" p ".time")))
                 (select-best-time lines)))
              (else o))))
         (player-colors
          (result-colors player-results))
         (player-deltas
          (result-deltas player-results)))
    (displayln "<tr>")
    (printf "<td>~a</td>~n" g)
    (for ((r player-results)
          (c player-colors)
          (d player-deltas))
      (if (number? r)
        (printf "<td/><td align=\"right\" style=\"background-color:~a\">~0,2f ~a</td>~n" c r d)
        (printf "<td/><td align=\"right\" style=\"color:purple\"> ~a</td>~n" r)))
    (displayln "</tr>")))

(def (select-best-time lines)
  (let lp ((rest lines) (best-time #f))
    (match rest
      ([line . rest]
       (let* ((numbers (map string->number (string-split line #\,)))
              (time (+ (car numbers) (cadr numbers))))
         (if best-time
           (if (< time best-time)
             (lp rest time)
             (lp rest best-time))
           (lp rest time))))
      (else best-time))))

(def (result-colors results)
  (let* ((indexed  (map (lambda (r i) (cons (if (number? r) r +inf.0) i))
                        results (iota (length results))))
         (sorted  (sort indexed (lambda (a b) (< (car a) (car b)))))
         (best    (caar sorted))
         (colored (map (lambda (x)
                         (cons
                          (let (delta (/ (- (car x) best) best))
                            (cond
                             ((zero? delta) "lawngreen")
                             ((< delta .1)  "lightgreen")
                             ((< delta .25) "lightcyan")
                             ((< delta .5)  "lightyellow")
                             ((< delta 1)   "yellow")
                             ((< delta 2)   "orange")
                             (else          "orangered")))
                          (cdr x)))
                       sorted))
         (sorted (sort colored (lambda (a b) (< (cdr a) (cdr b))))))
    (map car sorted)))

(def (result-deltas results)
  (let* ((timings (map (lambda (r) (if (number? r) r +inf.0)) results))
         (best (car (sort timings <))))
    (map (lambda (t)
           (if (> t best)
             (if (finite? t)
               (format "(+~a%)" (/ (floor (* 1000 (/ (- t best) best))) 10))
               (format "~a" t))
             "↻"))
         timings)))

(def (main)
  (call-with-output-file "table.html"
    generate-table))
