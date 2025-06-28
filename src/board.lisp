(in-package :alphaknight)

;; Board, struct
;;; defines:
;;;; make-Board :: make an instance of a Board
;;;; Board-p    :: check if symbol is a Board
;;; How to access a record:
;;;; Board-<NameOfRecord>
;;; How to access a single tile:
;;;; (elt (Board-<NameOfRecord> board) i)
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

(defvar *Board-members* '(WKnights WPawns WRooks WBishops WKings WQueens BPawns BKnights BBishops BRooks BKings BQueens))

(defvar *white-chars* '(:knight "♘" :pawn "♙" :rook "♖" :bishop "♗" :king "♔" :queen "♕"))
(defvar *black-chars* '(:pawn "♟" :knight "♞" :bishop "♝" :rook "♜" :king "♚" :queen "♛"))

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
  (setf (Board-WBishops b) #*0100001000000000000000000000000000000000000000000000000000000000)
  (setf (Board-BBishops b) #*0000000000000000000000000000000000000000000000000000000100001000))

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
  (init-rooks b)
  b)

(defun deep-copy-Board (b)
  (make-Board
  :WKnights (copy-seq (Board-WKnights b))
  :WPawns   (copy-seq (Board-WPawns b))
  :WRooks   (copy-seq (Board-WRooks b))
  :WBishops (copy-seq (Board-WBishops b))
  :WKings   (copy-seq (Board-WKings b))
  :WQueens  (copy-seq (Board-WQueens b))
  :BPawns   (copy-seq (Board-BPawns b))
  :BKnights (copy-seq (Board-BKnights b))
  :BBishops (copy-seq (Board-BBishops b))
  :BRooks   (copy-seq (Board-BRooks b))
  :BKings   (copy-seq (Board-BKings b))
  :BQueens  (copy-seq (Board-BQueens b))))

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

(defun print-board (board)
  (let ((array (make-array 64 :initial-element "."))
	(pieces '(:pawn :knight :bishop :rook :queen :king)))
    (dolist (p pieces)
      (let ((bitvec (get-White-piece p board)))
	(dotimes (i 64)
	  (when (= (elt bitvec i) 1)
	    (setf (aref array i) (getf *white-chars* p))))))
    (dolist (p pieces)
      (let ((bitvec (get-Black-piece p board)))
	(dotimes (i 64)
	  (when (= (elt bitvec i) 1)
	    (setf (aref array i) (getf *black-chars* p))))))
    (dotimes (i 64)
      (format t (aref array i))
      (when (= (mod  (1+ i) 8) 0)
	(terpri)))))

(defun rank (idx)
  "Dato un indice IDX ritorna il numero di rango corrispondente"
  (1+ (mod idx 8)))

(defun Pfile (f)
  "Dato un indice f corrispondente ad un rango, ritorna la lettera corrispondente"
  (cond ((= f 1) "A")
	((= f 2) "B")
	((= f 3) "C")
	((= f 4) "D")
	((= f 5) "E")
	((= f 6) "F")
	((= f 7) "G")
	((= f 8) "H")
	(t "A")))

(defun file (idx)
  (cond ((= (rem idx 8) 0) (1+ 0))
	((and (>= (rem idx 8) 1) (<= (rem idx 8) 7)) (1+ 1))))

(defun uci->idx (uci)
  (let ((f (car uci))
	(r (cadr uci)))
    (+ (* f 8) r)))

(defun idx->uci (idx)
  (let ((r 0)
	(f 0))
    (setf r (floor (/ idx 8)))
    (setf f (rem idx 8))
    (list f r)))

(defun Puci (uci)
  (let ((r (1+ (cadr uci)))
	(f (1+ (car uci))))
    (list (Pfile f) r)))


(defun opposite-color (color)
  (if (eq color :bianco) :nero :bianco))

(defun square-occupied-by-color (idx color board)
  "Check if square is occupied by specified color"
  (ecase color
    (:bianco (or (eql (elt (Board-WPawns board) idx) 1)
                 (eql (elt (Board-WKnights board) idx) 1)
                 (eql (elt (Board-WBishops board) idx) 1)
                 (eql (elt (Board-WRooks board) idx) 1)
                 (eql (elt (Board-WQueens board) idx) 1)
                 (eql (elt (Board-WKings board) idx) 1)))
    (:nero (or (eql (elt (Board-BPawns board) idx) 1)
               (eql (elt (Board-BKnights board) idx) 1)
               (eql (elt (Board-BBishops board) idx) 1)
               (eql (elt (Board-BRooks board) idx) 1)
               (eql (elt (Board-BQueens board) idx) 1)
               (eql (elt (Board-BKings board) idx) 1)))))

;;; Add to board.lisp
(defun get-king-square (board color)
  "Find king position for COLOR"
  (let ((king-vec (if (eq color :bianco)
                    (Board-WKings board)
                    (Board-BKings board))))
    (dotimes (i 64)
      (when (eql (elt king-vec i) 1)
        (return i)))
    nil))

    
;;; Complete get-piece-type-at function
(defun get-piece-type-at (idx board color)
  "Identify piece type at position"
  (cond 
    ((eql (elt (if (eq color :bianco) 
                 (Board-WPawns board) 
                 (Board-BPawns board))
	       idx)
	  1)
     :pawn)
    ((eql (elt (if (eq color :bianco) 
                 (Board-WKnights board) 
                 (Board-BKnights board))
	       idx)
	  1)
     :knight)
    ((eql (elt (if (eq color :bianco) 
                 (Board-WBishops board) 
                 (Board-BBishops board))
	       idx)
	  1)
     :bishop)
    ((eql (elt (if (eq color :bianco) 
                 (Board-WRooks board) 
                 (Board-BRooks board))
	       idx)
	  1)
     :rook)
    ((eql (elt (if (eq color :bianco) 
                 (Board-WQueens board) 
                 (Board-BQueens board))
	       idx)
	  1)
     :queen)
    ((eql (elt (if (eq color :bianco) 
                 (Board-WKings board) 
                 (Board-BKings board))
	       idx)
	  1)
     :king)
    (t nil)))
