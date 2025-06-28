(in-package :alphaknight)

;; Posizione, struct
;;; defines:
;;;; make-Posizione
;;;; Posizione-p
;;; How to access a record
;;;; Posizione-<NameOfRecord>
(defstruct Posizione
  board
  giocatore) ;; valori validi per giocatore: :bianco :nero
 
(defvar *Posizione-record-names*
  '(Posizione-Board Posizione-giocatore)) 

(defun init-Posizione (p)
  "Inizializza una posizione"
  (setf (Posizione-board p) (init-board (make-Board)))
  (setf (Posizione-giocatore p) :bianco)
  p)
