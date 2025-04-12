(in-package #:alphaknight)

(defvar piece-values
  '((:pawn   100)
    (:knight 320)
    (:bishop 330)
    (:rook   500)
    (:queen  900)
    (:king   20000)))
;; per ottenere il valore di un dato pezzo:
;; (cadr (assoc :pezzo piece-values))

(defstruct square-tables
  Pawns-values
  Knights-values
  Bishops-valuse
  Rooks-values
  Queens-values
  Kings-midgame-values
  Kings-endgame-values)

(defun init-square-tables (st)
  (setf (square-tables-Pawns-values st) '(0  0  0   0   0   0   0  0
					  5 10 10 -20 -20  10  10  5
					  5 -5 -10  0   0 -10  -5  5
					  0  0   0 20  20   0   0  0
					  5  5  10 25  25  10   5  5
					  10 10 20 30  30  20  10 10
					  50 50 50 50  50  50  50 50
					  0  0  0  0   0   0   0  0))
  (setf (square-tables-Rooks-values st) '( 0 0 0 5 5 0 0  0
					  -5 0 0 0 0 0 0 -5
					  -5 0 0 0 0 0 0 -5
					  -5 0 0 0 0 0 0 -5
					  -5 0 0 0 0 0 0 -5
					  -5 0 0 0 0 0 0 -5
					  5 10 10 10 10 10 10 5
					  0 0 0 0 0 0 0 0))
  (setf (square-tables-Bishops-values st) '(-20 -10 -10 -10 -10 -10 -10 -20
					    -10 5 0 0 0 0 5 -10
					    -10 10 10 10 10 10 10 -10
					    -10 0 10 10 10 10 0 -10
					    -10 5 5 10 10 5 5 -10
					    -10 0 5 10 10 5 0 -10
					    -10 0 0 0 0 0 0 -10
					    -20 -10 -10 -10 -10 -10 -10 -20))
  (setf (square-tables-Knights-values st) '(-50 -40 -30 -30 -30 -30 -40 -50
					    -40 -20 0 5 5 0 -20 -40
					    -30 5 10 15 15 10 5 -30
					    -30 0 15 20 20 15 0 -30
					    -30 5 15 20 20 15 5 -30
					    -30 0 10 15 15 10 0 -30
					    -40 -20 0 0 0 0 -20 -40
					    -50 -40 -30 -30 -30 -30 -40 -50))
  (setf (square-tables-Queens-values st) '(-20 -10 -10 -5 -5 -10 -10 -20
					   -10 0 0 0 0 5 0 -10
					   -10 0 5 5 5 5 5 -10
					   -5 0 5 5 5 5 0 0
					   -5 0 5 5 5 5 0 -5
					   -10 0 5 5 5 5 0 -10
					   -10 0 0 0 0 0 0 -10
					   -20 -10 -10 -5 -5 -10 -10 -20))
  (setf (square-tables-Kings-midgame-values st) '(20 30 10 0 0 10 30 20
						  20 20 0 0 0 0 20 20
						  -10 -20 -20 -20 -20 -20 -20 -10
						  -20 -30 -30 -40 -40 -30 -30 -20
						  -30 -40 -40 -50 -50 -40 -40 -30
						  -30 -40 -40 -50 -50 -40 -40 -30
						  -30 -40 -40 -50 -50 -40 -40 -30
						  -30 -40 -40 -50 -50 -40 -40 -30))
  (setf (square-tables-Kings-lategame-values st) '(-50 -30 -30 -30 -30 -30 -30 -50
						   -30 -30 0 0 0 0 -30 -30
						   -30 -10 20 30 30 20 -10 -30
						   -30 -10 30 40 40 30 -10 -30
						   -30 -10 30 40 40 30 -10 -30
						   -30 -10 20 30 30 20 -10 -30
						   -30 -20 -10 0 0 -10 -20 -30
						   -50 -40 -30 -20 -20 -30 -40 -50)))

;; Board, struct
;;; defines:
;;;; make-Board :: make an instance of a Board
;;;; Board-p    :: check if symbol is a Board
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
