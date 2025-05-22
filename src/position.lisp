(in-package #:alphaknight)

;; Position, struct
;;; defines:
;;;; make-Position
;;;; Position-p
;;; How to access a record
;;;; Position-<NameOfRecord>
(defstruct Position
  Board
  giocatore) ;; valori validi per giocatore: :bianco :nero
 
(defvar *Position-record-names*
  '(Position-Board Position-giocatore)) 

(defun init-Position (p)
  "Inizializza una posizione"
  (setf (Position-Board p) (init-board (make-Board)))
  (seft (Position-giocatore p) :bianco))
