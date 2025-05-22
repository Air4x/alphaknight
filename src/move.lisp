(in-package #:alphaknight)


(defun generate-white-pawn (idx)
  "Dato un indice e lo stato del gioco, genera tutte le mosse per un
  pedone bianco"
  ;; generazione spostamenti
  (let ((r (rank idx))
	(f (file idx)))
    (format t "Rank: ~D; File: ~D" r f)
    )
  ;; controllo cattura
  ;; ritorno posizioni
  )
