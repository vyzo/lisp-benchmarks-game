;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; directly ported from the go version
;;; comments from the original version
(import :std/error
        :std/sugar
        :std/iter
        :std/format
        :std/text/utf8
        :std/os/fdio)
(export main)
(declare
  (not safe)
  (fixnum))
(include "io.ss")
(include "vector.ss")

(def max-solutions 0)

(def (bool-int b)
  (if b 1 0))


;; The board is a 50 cell hexagonal pattern.  For    . . . . .
;; maximum speed the board will be implemented as     . . . . .
;; 50 bits, which will fit into a 64 bit long long   . . . . .
;; int.                                               . . . . .
;;                                                   . . . . .
;; I will represent 0's as empty cells and 1's        . . . . .
;; as full cells.                                    . . . . .
;;                                                    . . . . .
;;                                                   . . . . .
;;                                                    . . . . .
;;
;; Note (vyzo): one less octet, to fit in fixnum in 64 bit
(def board      #xFFC000000000000)

;; The puzzle pieces must be specified by the path followed
;; from one end to the other along 12 hexagonal directions.
;;
;;   Piece 0   Piece 1   Piece 2   Piece 3   Piece 4
;;
;;  O O O O    O   O O   O O O     O O O     O   O
;;         O    O O           O       O       O O
;;                           O         O         O
;;
;;   Piece 5   Piece 6   Piece 7   Piece 8   Piece 9
;;
;;    O O O     O O       O O     O O        O O O O
;;       O O       O O       O       O O O        O
;;                  O       O O
;;
;; I had to make it 12 directions because I wanted all of the
;; piece definitions to fit into the same size arrays.  It is
;; not possible to define piece 4 in terms of the 6 cardinal
;; directions in 4 moves.
;;

(def E      0)
(def ESE    1)
(def SE     2)
(def S      3)
(def SW     4)
(def WSW    5)
(def W      6)
(def WNW    7)
(def NW     8)
(def N      9)
(def NE    10)
(def ENE   11)
(def PIVOT 12)

(defsyntax (direction-case stx)
  (def (make-clause clause)
    (syntax-case clause (else)
      ((else body ...) clause)
      (((dir) body ...)
       (cond
        ((assgetq (stx-e #'dir)
                  '((E . 0)
                    (ESE . 1)
                    (SE . 2)
                    (S . 3)
                    (SW . 4)
                    (WSW . 5)
                    (W . 6)
                    (WNW . 7)
                    (NW . 8)
                    (N . 9)
                    (NE . 10)
                    (ENE . 11)))
         => (lambda (val) (with-syntax ((val val)) #'((val) body ...))))
        (else
         (raise-syntax-error #f "bad direction" stx #'dir))))))
  (syntax-case stx ()
    ((_ dir clause ...)
     (with-syntax (((clause ...) (stx-map make-clause #'(clause ...))))
       #'(case dir clause ...)))))

(def piece-def
  (vector
   (vector E E E SE)
   (vector SE E NE E)
   (vector E E SE SW)
   (vector E E SW SE)
   (vector SE E NE S)
   (vector E E SW E)
   (vector E SE SE NE)
   (vector E SE SE W)
   (vector E SE E E)
   (vector E E E SW)))

;; To minimize the amount of work done in the recursive solve function below,
;; I'm going to allocate enough space for all legal rotations of each piece
;; at each position on the board. That's 10 pieces x 50 board positions x
;; 12 rotations.  However, not all 12 rotations will fit on every cell, so
;; I'll have to keep count of the actual number that do.
;; The pieces are going to be unsigned long long ints just like the board so
;; they can be bitwise-anded with the board to determine if they fit.
;; I'm also going to record the next possible open cell for each piece and
;; location to reduce the burden on the solve function.
(def pieces
  (make-vector* 10 50 12 0))
(def piece-counts
  (make-vector* 10 50 0))
(def next-cell
  (make-vector* 10 50 12 0))

;; Returns the direction rotated 60 degrees clockwise
(def (rotate dir)
  (% (+ dir 2) PIVOT))

;; Returns the direction flipped on the horizontal axis
(def (flip dir)
  (% (- PIVOT dir) PIVOT))

;; Returns the new cell index from the specified cell in the
;; specified direction.  The index is only valid if the
;; starting cell and direction have been checked by the
;; out_of_bounds function first.
(def (shift cell dir)
  (defrule (cell/5%2)
    (% (// cell 5) 2))
  (direction-case dir
    ((E)
     (+ cell 1))
    ((ESE)
     (+ cell 6 (cell/5%2)))
    ((SE)
     (+ cell 5 (cell/5%2)))
    ((S)
     (+ cell 10))
    ((SW)
     (+ cell 4 (cell/5%2)))
    ((WSW)
     (+ cell 3 (cell/5%2)))
    ((W)
     (- cell 1))
    ((WNW)
     (- cell (- 7 (cell/5%2))))
    ((NW)
     (- cell (- 6 (cell/5%2))))
    ((N)
     (- cell 10))
    ((NE)
     (- cell (- 5 (cell/5%2))))
    ((ENE)
     (- cell (- 4 (cell/5%2))))
    (else cell)))

;; Returns wether the specified cell and direction will land outside
;; of the board.  Used to determine if a piece is at a legal board
;; location or not.
(def (out-of-bounds? cell dir)
  (defrule (%10) (% cell 10))
  (defrule (%5) (% cell 5))
  (direction-case dir
    ((E)
     (= (%5) 4))
    ((ESE)
     (case (%10)
       ((4 8 9) #t)
       (else (>= cell 45))))
    ((SE)
     (or (= (%10) 9) (>= cell 45)))
    ((S)
     (>= cell 40))
    ((SW)
     (or (= (%10) 0) (>= cell 45)))
    ((WSW)
     (case (%10)
       ((0 1 5) #t)
       (else (>= cell 45))))
    ((W)
     (= (%5) 0))
    ((WNW)
     (case (%10)
       ((0 1 5) #t)
       (else (< cell 5))))
    ((NW)
     (or (= (%10) 0) (< cell 5)))
    ((N)
     (< cell 10))
    ((NE)
     (or (= (%10) 9) (< cell 5)))
    ((ENE)
     (case (%10)
       ((4 8 9) #t)
       (else (< cell 5))))
    (else #f)))

;; Rotate a piece 60 degrees clockwise
(def (rotate-piece piece)
  (for (i (in-range 4))
    (set! (@@ piece-def piece i)
      (rotate (@@ piece-def piece i)))))

;; Flip a piece along the horizontal axis
(def (flip-piece piece)
  (for (i (in-range 4))
    (set! (@@ piece-def piece i)
      (flip (@@ piece-def piece i)))))

;; Convenience function to quickly calculate all of the indices for a piece
(def (calc-cell-indices cell piece index)
  (set! (@@ cell 0) index)
  (for (i (in-range 1 5))
    (set! (@@ cell i)
      (shift (@@ cell (- i 1))
             (@@ piece-def piece (- i 1))))))

;; Convenience function to quickly calculate if a piece fits on the board
(def (cells-fit-on-board? cell piece)
  (not (or (out-of-bounds? (@@ cell 0) (@@ piece-def piece 0))
           (out-of-bounds? (@@ cell 1) (@@ piece-def piece 1))
           (out-of-bounds? (@@ cell 2) (@@ piece-def piece 2))
           (out-of-bounds? (@@ cell 3) (@@ piece-def piece 3)))))

;; Returns the lowest index of the cells of a piece.
;; I use the lowest index that a piece occupies as the index for looking up
;; the piece in the solve function.
(def (minimum-of-cells cell)
  (min (@@ cell 0)
       (@@ cell 1)
       (@@ cell 2)
       (@@ cell 3)
       (@@ cell 4)))

;; Calculate the lowest possible open cell if the piece is placed on the board.
;; Used to later reduce the amount of time searching for open cells in the
;; solve function.
(def (first-empty-cell cell minimum)
  (let loop ((first-empty minimum))
    (if (or (= first-empty (@@ cell 0))
            (= first-empty (@@ cell 1))
            (= first-empty (@@ cell 2))
            (= first-empty (@@ cell 3))
            (= first-empty (@@ cell 4)))
      (loop (+ first-empty 1))
      first-empty)))

;; Generate the unsigned long long int that will later be anded with the
;; board to determine if it fits.
(def (bitmask-from-cells cell)
  (let (piece-mask 0)
    (for (i (in-range 5))
      (set! piece-mask
        (fxior piece-mask (<< 1 (@@ cell i)))))
    piece-mask))

;; Record the piece and other important information in arrays that will
;; later be used by the solve function.
(def (record-piece piece minimum first-empty piece-mask)
  (let (pcm (@@ piece-counts piece minimum))
    (set! (@@ pieces piece minimum pcm) piece-mask)
    (set! (@@ next-cell piece minimum pcm) first-empty)
    (set! (@@ piece-counts piece minimum) (+ pcm 1))))

;; Fill the entire board going cell by cell.  If any cells are "trapped"
;; they will be left alone.
(def (fill-contiguous-space board index)
  (unless (= (@@ board index) 1)
    (set! (@@ board index) 1)
    (unless (out-of-bounds? index E)
      (fill-contiguous-space board (shift index E)))
    (unless (out-of-bounds? index SE)
      (fill-contiguous-space board (shift index SE)))
    (unless (out-of-bounds? index SW)
      (fill-contiguous-space board (shift index SW)))
    (unless (out-of-bounds? index W)
      (fill-contiguous-space board (shift index W)))
    (unless (out-of-bounds? index NW)
      (fill-contiguous-space board (shift index NW)))
    (unless (out-of-bounds? index NE)
      (fill-contiguous-space board (shift index NE)))))

;; To thin the number of pieces, I calculate if any of them trap any empty
;; cells at the edges.  There are only a handful of exceptions where the
;; the board can be solved with the trapped cells.  For example:  piece 8 can
;; trap 5 cells in the corner, but piece 3 can fit in those cells, or piece 0
;; can split the board in half where both halves are viable.
(def (has-island? cell piece)
  (let (temp-board (make-vector 50 0))
    (for (i (in-range 5))
      (set! (@@ temp-board (@@ cell i)) 1))
    (let loop ((i 49))
      (if (= (@@ temp-board i) 1)
        (loop (- i 1))
        (fill-contiguous-space temp-board i)))
    (let (c 0)
      (for (i (in-range 50))
        (when (= (@@ temp-board i) 0)
          (set! c (+ c 1))))
      (not (or (= c 0)
               (and (= c 5) (= piece 8))
               (and (= c 40) (= piece 8))
               (and (= (% c 5) 0) (= piece 0)))))))

;; Calculate all six rotations of the specified piece at the specified index.
;; We calculate only half of piece 3's rotations.  This is because any solution
;; found has an identical solution rotated 180 degrees.  Thus we can reduce the
;; number of attempted pieces in the solve algorithm by not including the 180-
;; degree-rotated pieces of ONE of the pieces.  I chose piece 3 because it gave
;; me the best time ;)
(def (calc-six-rotations piece index)
  (let (cell (make-vector 5 0))
    (for (rotation (in-range 6))
      (when (or (not (= piece 3)) (< rotation 3))
        (calc-cell-indices cell piece index)
        (when (and (cells-fit-on-board? cell piece)
                   (not (has-island? cell piece)))
          (let* ((minimum (minimum-of-cells cell))
                 (first-empty (first-empty-cell cell minimum))
                 (piece-mask (bitmask-from-cells cell)))
            (record-piece piece minimum first-empty piece-mask))))
      (rotate-piece piece))))

;; Calculate every legal rotation for each piece at each board location.
(def (calc-pieces)
  (for (piece (in-range 10))
    (for (index (in-range 50))
      (calc-six-rotations piece index)
      (flip-piece piece)
      (calc-six-rotations piece index))))

;; Calculate all 32 possible states for a 5-bit row and all rows that will
;; create islands that follow any of the 32 possible rows.  These pre-
;; calculated 5-bit rows will be used to find islands in a partially solved
;; board in the solve function.
(def ROW-MASK    #x1f)
(def TRIPLE-MASK #x7FFF)

(def all-rows
  (list->vector (iota 32)))
(def bad-even-rows
  (make-vector* 32 32 0))
(def bad-odd-rows
  (make-vector* 32 32 0))
(def bad-even-triple
  (make-vector 32768 0))
(def bad-odd-triple
  (make-vector 32768 0))

(def (rows-bad row1 row2 even?)
  (let/cc return
    (let* ((row2-shift
            (if even?
              (fxior (fxand (<< row2 1) ROW-MASK) #x01)
              (fxior (>> row2 1) #x10)))
           (block (fxand
                   (fxand (fxxor row1 row2) row2)
                   (fxand (fxxor row1 row2-shift) row2-shift)))
           (in-zeros? #f)
           (group-ok? #f))
      (for (i (in-range 5))
        (if (= (fxand row1 (<< 1 i)) 0)
          (begin
            (set! in-zeros? #t)
            (when (= (fxand block (<< 1 i)) 0)
              (set! group-ok? #t)))
          (when in-zeros?
            (unless group-ok?
              (return 1))
            (set! in-zeros? #f)
            (set! group-ok? #f))))
      (if in-zeros?
        (bool-int (not group-ok?))
        0))))

;; Check for cases where three rows checked sequentially cause a false
;; positive.  One scenario is when 5 cells may be surrounded where piece 5
;; or 7 can fit.  The other scenario is when piece 2 creates a hook shape.
(def (triple-is-ok? row1 row2 row3 even?)
  (if even?
    ;; There are four cases:
    ;; row1: 00011  00001  11001  10101
    ;; row2: 01011  00101  10001  10001
    ;; row3: 011??  00110  ?????  ?????
    (or (and (= row1 #x03) (= row2 #x0B) (= (fxand row3 #x1C) #x0C))
        (and (= row1 #x01) (= row2 #x05) (= row3 #x06))
        (and (= row1 #x19) (= row2 #x11))
        (and (= row1 #x15) (= row2 #x11)))
    ;; There are two cases:
    ;; row1: 10011  10101
    ;; row2: 10001  10001
    ;; row3: ?????  ?????
    (or (and (= row1 #x13) (= row2 #x11))
        (and (= row1 #x15) (= row2 #x11)))))

(def (calc-rows)
  (for (row1 (in-range 32))
    (for (row2 (in-range 32))
      (set! (@@ bad-even-rows row1 row2) (rows-bad row1 row2 #t))
      (set! (@@ bad-odd-rows row1 row2)   (rows-bad row1 row2 #f))))
  (for (row1 (in-range 32))
    (for (row2 (in-range 32))
      (for (row3 (in-range 32))
        (let ((result1 (@@ bad-even-rows row1 row2))
              (result2 (@@ bad-odd-rows row2 row3)))
          (if (and (= result1 0) (not (= result2 0)) (triple-is-ok? row1 row2 row3 #t))
            (set! (@@ bad-even-triple (+ row1 (* row2 32) (* row3 1024))) 0)
            (set! (@@ bad-even-triple (+ row1 (* row2 32) (* row3 1024)))
              (bool-int (not (and (= result1 0) (= result2 0))))))

          (set! result1 (@@ bad-odd-rows row1 row2))
          (set! result2 (@@ bad-even-rows row2 row3))
          (if (and (= result1 0) (not (= result2 0)) (triple-is-ok? row1 row2 row3 #f))
            (set! (@@ bad-odd-triple (+ row1 (* row2 32) (* row3 1024))) 0)
            (set! (@@ bad-odd-triple (+ row1 (* row2 32) (* row3 1024)))
              (bool-int (not (and (= result1 0) (= result2 0)))))))))))

;; Calculate islands while solving the board.
(def (board-has-islands cell)
  (if (>= cell 40)
    0
    (let (current-triple
          (fxand (>> board (* (// cell 5) 5))
                 TRIPLE-MASK))
      (if (= (% (// cell 5) 2) 0)
        (@@ bad-even-triple current-triple)
        (@@ bad-odd-triple current-triple)))))

;; The recursive solve algorithm.  Try to place each permutation in the upper-
;; leftmost empty cell.  Mark off available pieces as it goes along.
;; Because the board is a bit mask, the piece number and bit mask must be saved
;; at each successful piece placement.  This data is used to create a 50 char
;; array if a solution is found.
(def avail #x03FF)
(def sol-nums
  (make-vector 10 0))
(def sol-masks
  (make-vector 10 0))
(def solutions
  (make-vector* 2100 50 0))
(def solution-count 0)

(def (record-solution)
  (for (sol-no (in-range 10))
    (let (sol-mask (@@ sol-masks sol-no))
      (for (index (in-range 50))
        (when (= (fxand sol-mask 1) 1)
          (set! (@@ solutions solution-count index)
            (@@ sol-nums sol-no))
          (set! (@@ solutions (+ solution-count 1) (- 49 index))
            (@@ sol-nums sol-no)))
        (set! sol-mask (>> sol-mask 1)))))
  (set! solution-count (+ solution-count 2)))

(def (solve depth cell)
  (when (< solution-count max-solutions)
    (while (not (= (fxand board (<< 1 cell)) 0))
      (set! cell (+ cell 1)))
    (let loop-piece ((piece 0))
      (when (< piece 10)
        (let (piece-no-mask (<< 1 piece))
          (if (= (fxand avail piece-no-mask) 0)
            (loop-piece (+ piece 1))
            (begin
              (set! avail (fxxor avail piece-no-mask))
              (let ((max-rots (@@ piece-counts piece cell))
                    (piece-mask (@@ pieces piece cell)))
                (let loop-rot ((rotation 0))
                  (if (< rotation max-rots)
                    (if (= (fxand board (@@ piece-mask rotation)) 0)
                      (begin
                        (set! (@@ sol-nums depth) piece)
                        (set! (@@ sol-masks depth) (@@ piece-mask rotation))
                        (if (= depth 9)
                          ;; Solution found!!!!!11!!ONE!
                          (begin
                            (record-solution)
                            (set! avail (fxxor avail piece-no-mask)))
                          (begin
                            (set! board
                              (fxior board (@@ piece-mask rotation)))
                            (when (= (board-has-islands (@@ next-cell piece cell rotation)) 0)
                              (solve (+ depth 1) (@@ next-cell piece cell rotation)))
                            (set! board
                              (fxxor board (@@ piece-mask rotation)))
                            (loop-rot (+ rotation 1)))))
                      (loop-rot (+ rotation 1)))
                    (begin
                      (set! avail (fxxor avail piece-no-mask))
                      (loop-piece (+ piece 1)))))))))))))

;; pretty print a board in the specified hexagonal format
(def (pretty b)
  (for (i (in-range 0 50 10))
    (defrule (write-char@ j)
      (begin
        (write-output-u8 (+ (@@ b (+ i j)) (char->integer #\0)))
        (write-output-char #\space)))
    (write-char@ 0) (write-char@ 1) (write-char@ 2) (write-char@ 3) (write-char@ 4)
    (write-output-newline) (write-output-char #\space)
    (write-char@ 5) (write-char@ 6) (write-char@ 7) (write-char@ 8) (write-char@ 9)
    (write-output-newline))
  (write-output-newline))

;; Find smallest and largest solutions
(def (smallest-largest)
  (let ((smallest (@@ solutions 0))
        (largest  (@@ solutions 0)))
    (for (i (in-range 1 solution-count))
      (let (candidate (@@ solutions i))
        (let loop ((j 0))
          (when (< j 50)
            (let ((s (@@ smallest j))
                  (c (@@ candidate j)))
              (cond
               ((= c s) (loop (+ j 1)))
               ((< c s)
                (set! smallest candidate))))))
        (let loop ((j 0))
          (when (< j 50)
            (let ((s (@@ largest j))
                  (c (@@ candidate j)))
              (cond
               ((= c s) (loop (+ j 1)))
               ((> c s)
                (set! largest candidate))))))))
    (values smallest largest)))

(def (main n)
  (let (n (string->number n))
    (set! max-solutions n)
    (calc-pieces)
    (calc-rows)
    (solve 0 0)
    (write-output-string (format "~a solutions found\n\n" solution-count))
    (let ((values smallest largest)
          (smallest-largest))
      (pretty smallest)
      (pretty largest))
    (flush-output)))
