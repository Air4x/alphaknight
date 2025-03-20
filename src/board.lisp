(in-package #:alphaknight)

;;   A B C D E F G H
;; 8 R B N Q K N B R
;; 7 P P P P P P P P
;; 6
;; 5
;; 4
;; 3
;; 2 P P P P P P P P
;; 1 R B N Q K N B R   


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


;; (defun pawns->letter (b)
;;   (dolist #'(lambda (x)
;; 	      (cond ((= 1 x) "P")
;; 		    ((t 'nil))))))

;; (defun print-board (b)
;;   (let ((to-print '()))
;;     ))
