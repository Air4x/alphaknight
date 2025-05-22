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

(defun valuta-posizione (posizione piece-values square-tables)
  "Data una posizione, la valuta in relazione al giocatore che
  deve giocate ed utilizza i valori dei pezzi e la loro posizione
  per ottenere il punteggio, che viene restituito"
  (let ((score 0)
	(pezzi (:pawn :knight :bishop :rook :queen :king))
	(giocatore (Posizione-giocatore posizione)))
    (dolist (pezzo pezzi)
      (let* ((bianco (get-White-piece pezzo posizione))
	     (nero (get-Black-piece pezzo posizione))
	     (valore-pezzo (cadr (assoc piece-values pezzo))))
	(dotimes (i 64)
	  (when (= 1 (elt bianco i))
	    ;; Se il giocatore è nero inverti l'indice della square table
	    (let ((indice (if (eq giocatore :nero) (- 63 i) i)))
	      (incf score valore-pezzo)
	      (incf score (nth indice square-table)))
	    (if (eq giocatore :nero)
		(- score)
		score)
	    ))))))

