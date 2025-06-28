(in-package :alphaknight)


;;; Pawn Moves (White)
(defun generate-white-pawn (idx position)
  (let* ((board (Posizione-Board position))
         (coord (idx->uci idx))
         (f0 (first coord))
         (r0 (second coord))
         (moves '()))
    ;; Single/Double push
    (when (< r0 7)
      (unless (square-occupied-p (uci->idx (list f0 (1+ r0))) board)
        (push (list idx (uci->idx (list f0 (1+ r0)))) moves))
        (when (and (= r0 1) 
                   (not (square-occupied-p (uci->idx (list f0 (+ r0 2))) board)))
          (push (list idx (uci->idx (list f0 (+ r0 2)))) moves)))
    
    ;; Captures
    (dolist (f-offset '(-1 1))
      (let ((target-f (+ f0 f-offset)))
        (when (and (<= 0 target-f 7) (< r0 7))
          (let ((target-idx (uci->idx (list target-f (1+ r0)))))
            (when (square-occupied-p target-idx board)
              (push (list idx target-idx :capture) moves))))))
    moves))

;;; Knight Moves
(defun generate-knight-moves (idx position)
  (let* ((board (Posizione-Board position))
         (color (Posizione-giocatore position))
         (opponent (if (eq color :bianco) :nero :bianco))
         (moves '()))
    (dolist (move '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))
      (let* ((coord (idx->uci idx))
             (new-f (+ (first coord) (first move)))
             (new-r (+ (second coord) (second move)))
             (new-idx (uci->idx (list new-f new-r))))
        (when (and (<= 0 new-f 7) (<= 0 new-r 7))
          (cond
            ((not (square-occupied-p new-idx board))
             (push (list idx new-idx) moves))
            ((square-occupied-by-color new-idx opponent board)
             (push (list idx new-idx :capture) moves))))))
    moves))

;;; Generic Sliding Moves
(defun generate-sliding-moves (idx directions position)
  (let* ((board (Posizione-Board position))
         (color (Posizione-giocatore position))
         (opponent (if (eq color :bianco) :nero :bianco))
         (moves '()))
    (dolist (dir directions)
      (let* ((coord (idx->uci idx))
             (f0 (first coord))
             (r0 (second coord))
             (df (first dir))
             (dr (second dir)))
        (loop
          (setf f0 (+ f0 df))
          (setf r0 (+ r0 dr))
          (when (or (< f0 0) (> f0 7) (< r0 0) (> r0 7)) (return))
          (let* ((new-idx (uci->idx (list f0 r0))))
            (cond
              ((square-occupied-by-color new-idx color board) (return))
              ((square-occupied-by-color new-idx opponent board)
               (push (list idx new-idx :capture) moves) (return))
              (t (push (list idx new-idx) moves))))))
    moves)))

;;; Dispatch to Piece Generators
(defun generate-moves-for-piece (idx position)
  (let* ((board (Posizione-Board position))
         (color (Posizione-giocatore position))
         (piece-type (get-piece-type-at idx board color)))
    (ecase piece-type
      (:pawn (if (eq color :bianco)
              (generate-white-pawn idx position)
              (generate-black-pawn idx position)))
      (:knight (generate-knight-moves idx position))
      (:bishop (generate-sliding-moves idx '((-1 -1) (-1 1) (1 -1) (1 1)) position))
      (:rook (generate-sliding-moves idx '((-1 0) (1 0) (0 -1) (0 1)) position))
      (:queen (generate-sliding-moves idx '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) position))
      (:king (generate-king-moves idx position)))))


;;; Bishop Moves (Diagonal)
(defun generate-bishop-moves (idx position)
  (let* ((board (Posizione-Board position))
         (color (Posizione-giocatore position))
         (opponent (opposite-color color))
         (moves '())
         (start-coord (idx->uci idx)))
    (dolist (dir '((-1 -1) (-1 1) (1 -1) (1 1)) moves)
      (let ((f (first start-coord))
            (r (second start-coord))
            (df (first dir))
            (dr (second dir)))
        (loop
          (setf f (+ f df))
          (setf r (+ r dr))
          (when (or (< f 0) (> f 7) (< r 0) (> r 7)) (return))
          (let* ((new-idx (uci->idx (list f r))))
            (cond
              ((square-occupied-by-color new-idx color board) (return))
              ((square-occupied-by-color new-idx opponent board)
               (push (list idx new-idx :capture) moves) (return))
              (t (push (list idx new-idx) moves)))))))))

;;; Rook Moves (Horizontal/Vertical)
(defun generate-rook-moves (idx position)
  (let* ((board (Posizione-Board position))
         (color (Posizione-giocatore position))
         (opponent (opposite-color color))
         (moves '())
         (start-coord (idx->uci idx)))
    (dolist (dir '((-1 0) (1 0) (0 -1) (0 1)) moves)
      (let ((f (first start-coord))
            (r (second start-coord))
            (df (first dir))
            (dr (second dir)))
        (loop
          (setf f (+ f df))
          (setf r (+ r dr))
          (when (or (< f 0) (> f 7) (< r 0) (> r 7)) (return))
          (let* ((new-idx (uci->idx (list f r))))
            (cond
              ((square-occupied-by-color new-idx color board) (return))
              ((square-occupied-by-color new-idx opponent board)
               (push (list idx new-idx :capture) moves) (return))
              (t (push (list idx new-idx) moves)))))))))

;;; Queen Moves (Diagonal + Horizontal/Vertical)
(defun generate-queen-moves (idx position)
  (let* ((board (Posizione-Board position))
         (color (Posizione-giocatore position))
         (opponent (opposite-color color))
         (moves '())
         (start-coord (idx->uci idx)))
    (dolist (dir '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) moves)
      (let ((f (first start-coord))
            (r (second start-coord))
            (df (first dir))
            (dr (second dir)))
        (loop
          (setf f (+ f df))
          (setf r (+ r dr))
          (when (or (< f 0) (> f 7) (< r 0) (> r 7)) (return))
          (let* ((new-idx (uci->idx (list f r))))
            (cond
              ((square-occupied-by-color new-idx color board) (return))
              ((square-occupied-by-color new-idx opponent board)
               (push (list idx new-idx :capture) moves) (return))
              (t (push (list idx new-idx) moves)))))))))

;;; King Moves (All directions, no castling)
(defun generate-king-moves (idx position)
  (let* ((board (Posizione-Board position))
         (color (Posizione-giocatore position))
         (opponent (opposite-color color))
         (moves '())
         (start-coord (idx->uci idx))
         (directions '((-1 -1) (-1 0) (-1 1) 
                     (0 -1)           (0 1)
                     (1 -1)  (1 0)  (1 1))))
    (dolist (dir directions moves)
      (let* ((new-f (+ (first start-coord) (first dir)))
             (new-r (+ (second start-coord) (second dir))))
        (when (and (<= 0 new-f 7) (<= 0 new-r 7))
          (let* ((new-idx (uci->idx (list new-f new-r))))
            (cond
              ((square-occupied-by-color new-idx color board) nil) ; Skip own pieces
              ((square-occupied-by-color new-idx opponent board)
               (push (list idx new-idx :capture) moves))
              (t (push (list idx new-idx) moves)))))))))



;;; Helper Functions for Move Application
(defun set-piece (board idx color piece-type value)
  "Set the bitvector value for a specific piece type and color"
  (let ((bitvec (if (eq color :bianco)
                  (get-White-piece piece-type board)
                  (get-Black-piece piece-type board))))
    (when bitvec
      (setf (elt bitvec idx) value))))

(defun clear-square (board idx color)
  "Clear all pieces of a given color from a square"
  (dolist (piece '(:pawn :knight :bishop :rook :queen :king))
    (set-piece board idx color piece 0)))

(defun apply-move (position move)
  "Apply a move to the position and return a new position"
  (let* ((start-idx (first move))
         (end-idx (second move))
         (is-capture (third move))
         (board (deep-copy-Board (Posizione-Board position)))
         (current-player (Posizione-giocatore position))
         (opponent (opposite-color (Posizione-giocatore position)))
         (piece-type (get-piece-type-at start-idx board current-player)))
    
    ;; Handle capture
    (when (and is-capture (eq is-capture :capture))
      (clear-square board end-idx opponent))
    
    ;; Move the piece
    (set-piece board start-idx current-player piece-type 0)
    (set-piece board end-idx current-player piece-type 1)
    
    ;; Return new position with toggled player
    (make-Posizione :Board board :giocatore opponent)))


(defun generate-pseudo-legal-moves (position)
  (let* ((board (Posizione-board position))
         (color (Posizione-giocatore position))
         (moves '()))
    ;; Iterate through all squares
    (dotimes (idx 64)
      (when (square-occupied-by-color idx color board)
        (setf moves 
              (append moves 
                      (generate-moves-for-piece idx position)))))
    moves))

;;; Generate legal moves with king safety check
(defun generate-legal-moves (position)
  (let ((pseudo-moves (generate-pseudo-legal-moves position))
        (current-player (Posizione-giocatore position)))
    (remove-if (lambda (move)
                 (let* ((new-pos (apply-move position move))
                        (king-pos (get-king-square (Posizione-board new-pos) current-player)))
                   (king-in-check-p (make-Posizione 
                                    :Board (Posizione-board new-pos)
                                    :giocatore current-player))))
               pseudo-moves)))


 ;;; Add to move.lisp
(defun generate-black-pawn (idx position)
  (let* ((board (Posizione-Board position))
         (coord (idx->uci idx))
         (f0 (first coord))
         (r0 (second coord))
         (moves '()))
    ;; Single/Double push
    (when (> r0 0)
      (unless (square-occupied-p (uci->idx (list f0 (1- r0))) board))
        (push (list idx (uci->idx (list f0 (1- r0)))) moves)
        (when (and (= r0 6) 
                   (not (square-occupied-p (uci->idx (list f0 (- r0 2))) board)))
          (push (list idx (uci->idx (list f0 (- r0 2)))) moves)))
    
    ;; Captures
    (dolist (f-offset '(-1 1))
      (let ((target-f (+ f0 f-offset)))
        (when (and (<= 0 target-f 7) (> r0 0))
          (let ((target-idx (uci->idx (list target-f (1- r0)))))
            (when (square-occupied-p target-idx board)
              (push (list idx target-idx :capture) moves))))))
    moves))
    

  
