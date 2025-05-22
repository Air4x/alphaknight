(in-package #:alphaknight)

;; Board, struct
;;; defines:
;;;; make-Board :: make an instance of a Board
;;;; Board-p    :: check if symbol is a Board
;;; How to access a record:
;;;; Board-<NameOfRecord>
;;; How to access a single tile:
;;;; (elt (Board-<NameOfRecord> board))
(defstruct Board
  WKnights
  WPawns
  WRooks
  WBishops
  WKings
  WQueens
  BPawns
  BKnights
  BBishops
  BRooks
  BKings
  BQueens)


(defun init-pawns (b)
  (setf (Board-WPawns b) #*0000000011111111000000000000000000000000000000000000000000000000)
  (setf (Board-BPawns b) #*0000000000000000000000000000000000000000000000001111111100000000))

(defun init-rooks (b)
  (setf (Board-WRooks b) #*1000000100000000000000000000000000000000000000000000000000000000)
  (setf (Board-BRooks b) #*0000000000000000000000000000000000000000000000000000000010000001))

(defun init-knights (b)
  (setf (Board-WKnights b) #*0010010000000000000000000000000000000000000000000000000000000000)
  (setf (Board-BKnights b) #*0000000000000000000000000000000000000000000000000000000000100100))

(defun init-bishops (b)
  (setf (Board-WBishops b) #*01000010000000000000000000000000000000000000000000000000000000)
  (setf (Board-BBishops b) #*00000000000000000000000000000000000000000000000000000001000010))

(defun init-kings (b)
  (setf (Board-WKings b) #*0000100000000000000000000000000000000000000000000000000000000000)
  (setf (Board-BKings b) #*0000000000000000000000000000000000000000000000000000000000001000))

(defun init-queens (b)
  (setf (Board-WQueens b) #*0001000000000000000000000000000000000000000000000000000000000000)
  (setf (Board-BQueens b) #*0000000000000000000000000000000000000000000000000000000000010000))

(defun init-board (b)
  (init-pawns b)
  (init-bishops b)
  (init-kings b)
  (init-knights b)
  (init-queens b)
  (init-rooks b))

(defun get-White-piece (piece board)
  "Data una keyword ritorna la posizione dei pezzi corrispondenti
al bianco"
  (cond ((eq piece :pawn)    (Board-WPawns board))
	((eq piece :knight)  (Board-WKnights board))
	((eq piece :bishop)  (Board-WBishops board))
	((eq piece :rook)    (Board-WRooks board))
	((eq piece :queen)   (Board-WQueens board))
	((eq piece :king)    (Board-WKings board))))

(defun get-Black-piece (piece board)
  "Data una keyword ritorna la posizione dei pezzi corrispondenti
al bianco"
  (cond ((eq piece :pawn)    (Board-BPawns board))
	((eq piece :knight)  (Board-BKnights board))
	((eq piece :bishop)  (Board-BBishops board))
	((eq piece :rook)    (Board-BRooks board))
	((eq piece :queen)   (Board-BQueens board))
	((eq piece :king)    (Board-BKings board))))

(defmacro make-PBoard (f b members chars)
  `(progn
     ,@(loop for m in members
	     for c in chars
	     collect `(,f (,m ,b) ,c))))

(defun print-PBoard (pb)
  (loop for i from 0 below (length pb)
	do (format t "~A" (nth i pb))
	(when (= (mod (1+ i) 8) 0)
	  (terpri))))

(defun bitvector->PBoard (v pb char)
  "Add the characters to pb depending on the bitvector, a 0 is a space a
1 is a specific character"
  (loop for idx from 0
	for bit in v
	do (cond ((= idx 1) (setf (nth idx v) char))
		 ((= idx 0) (setf (nth idx v) " ")))))

(defun rank (idx)
  "Dato un indice IDX ritorna il numero di rango corrispondente"
  (1+ (mod idx 8)))

(defun Prank (r)
  "Dato un indice R corrispondente ad un rango, ritorna la lettera corrispondente"
  (cond ((= r 1) "A")
	((= r 2) "B")
	((= r 3) "C")
	((= r 4) "D")
	((= r 5) "E")
	((= r 6) "F")
	((= r 7) "G")
	((= r 8) "H")
	(t "A")))
(defun file (idx)
  (cond ((= (rem idx 8) 0) (1+ 0))
	((and (>= (rem idx 8) 1) (<= (rem idx 8) 7)) (1+ 1))))

(defun uci->idx (sq)
  (- (* (rank sq) 8) (- (file sq) 8)))

(defun idx->uci (idx)
  (let ((f 0)
	(r 0))
    (+ (+ (floor (/ idx 8)) (file idx)) f)
    (+ (mod idx 8) r))
  '(f r))
