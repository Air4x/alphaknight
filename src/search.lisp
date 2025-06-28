(in-package :alphaknight)

;; Define constants for infinity
(defconstant +max-score+ most-positive-fixnum)
(defconstant +min-score+ most-negative-fixnum)

(defun terminal-score (position root-player)
  "Calculate score for terminal positions (checkmate/stalemate)"
  (let ((result (terminalp position)))
    (cond ((eq result :checkmate)
           (if (eq (Posizione-giocatore position) root-player)
               +min-score+   ; Root player is checkmated
               +max-score+)) ; Root player checkmated opponent
          ((eq result :stalemate)
           0)  ; Draw
          (t 0)))) ; Shouldn't happen, but return 0

(defun minimax (position depth alpha beta root-player)
  "Minimax algorithm with alpha-beta pruning"
  (if (or (zerop depth) (terminalp position))
      (if (terminalp position)
          (terminal-score position root-player)
          (valuta-posizione position piece-values *square-tables*))
      (let ((moves (generate-legal-moves position))
            (current-player (Posizione-giocatore position)))
        (if (eq current-player root-player)
            ;; Maximizing player (root player)
            (let ((best-value +min-score+))
              (loop for move in moves do
                (let* ((new-pos (apply-move position move))
                       (value (minimax new-pos (1- depth) alpha beta root-player)))
                (setf best-value (max best-value value))
                (setf alpha (max alpha best-value))
                (when (<= beta alpha)
                  (return))))
              best-value)
            ;; Minimizing player (opponent)
            (let ((best-value +max-score+))
              (loop for move in moves do
                (let* ((new-pos (apply-move position move))
                       (value (minimax new-pos (1- depth) alpha beta root-player)))
                (setf best-value (min best-value value))
                (setf beta (min beta best-value))
                (when (<= beta alpha)
                  (return))))
              best-value)))))

(defun find-best-move (position depth)
  "Find the best move using minimax with alpha-beta pruning"
  (let ((root-player (Posizione-giocatore position))
        (best-move nil)
        (best-value +min-score+)
        (alpha +min-score+)
        (beta +max-score+)
        (moves (generate-legal-moves position)))
    (dolist (move moves)
      (let* ((new-pos (apply-move position move))
             (value (minimax new-pos (1- depth) alpha beta root-player)))
        (when (> value best-value)
          (setf best-value value)
          (setf best-move move))
        (setf alpha (max alpha best-value))))
    (values best-move best-value)))
