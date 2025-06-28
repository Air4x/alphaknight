(in-package :alphaknight)

;;; Helper Functions
(defun square-occupied-p (idx board)
  "Check if any piece occupies square IDX"
  (or (eql (elt (Board-WPawns board) idx) 1)
      (eql (elt (Board-WKnights board) idx) 1)
      (eql (elt (Board-WBishops board) idx) 1)
      (eql (elt (Board-WRooks board) idx) 1)
      (eql (elt (Board-WQueens board) idx) 1)
      (eql (elt (Board-WKings board) idx) 1)
      (eql (elt (Board-BPawns board) idx) 1)
      (eql (elt (Board-BKnights board) idx) 1)
      (eql (elt (Board-BBishops board) idx) 1)
      (eql (elt (Board-BRooks board) idx) 1)
      (eql (elt (Board-BQueens board) idx) 1)
      (eql (elt (Board-BKings board) idx) 1)))

(defun is-piece-at? (idx piece-type color board)
  "Check if PIECE-TYPE of COLOR exists at IDX"
  (let ((bitvector (if (eq color :bianco)
                     (get-White-piece piece-type board)
                     (get-Black-piece piece-type board))))
    (and bitvector (eql (elt bitvector idx) 1))))

(defun get-king-square (board color)
  "Find king position for COLOR"
  (let ((king-vec (if (eq color :bianco)
                    (Board-WKings board)
                    (Board-BKings board))))
    (dotimes (i 64)
      (when (eql (elt king-vec i) 1)
        (return i)))
    nil))

;;; Attack Detection Functions
(defun ray-attack (square directions piece-type board attacker-color)
  "Check for attacks along rays"
  (let* ((coord (idx->uci square))
         (f0 (first coord))
         (r0 (second coord)))
    (dolist (dir directions)
      (let ((df (first dir))
            (dr (second dir))
            (f f0)
            (r r0))
        (loop
          (setf f (+ f df))
          (setf r (+ r dr))
          (when (or (< f 0) (> f 7) (< r 0) (> r 7)) (return))
          (let ((idx (uci->idx (list f r))))
            (when (square-occupied-p idx board)
              (return (is-piece-at? idx piece-type attacker-color board))))))
      nil)))

(defun king-in-check-p (position)
  "Main king safety check"
  (let* ((board (Posizione-board position))
         (color (Posizione-giocatore position))
         (king-square (get-king-square board color))
         (attacker (if (eq color :bianco) :nero :bianco)))
    (when king-square
      (or
       ;; Pawn attacks
       (let* ((coord (idx->uci king-square))
              (f (first coord))
              (r (second coord)))
         (if (eq color :bianco)
             (or (and (>= (1- f) 0) (<= (1+ r) 7)
                     (is-piece-at? (uci->idx (list (1- f) (1+ r))) :pawn attacker board))
                 (and (<= (1+ f) 7) (<= (1+ r) 7)
                     (is-piece-at? (uci->idx (list (1+ f) (1+ r))) :pawn attacker board)))
             (or (and (>= (1- f) 0) (>= (1- r) 0)
                     (is-piece-at? (uci->idx (list (1- f) (1- r))) :pawn attacker board))
                 (and (<= (1+ f) 7) (>= (1- r) 0)
                     (is-piece-at? (uci->idx (list (1+ f) (1- r))) :pawn attacker board)))))
       
       ;; Knight attacks
       (dolist (move '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)) nil)
         (let* ((new-f (+ (first (idx->uci king-square)) (first move)))
                (new-r (+ (second (idx->uci king-square)) (second move)))
                (new-idx (uci->idx (list new-f new-r))))
           (when (and (<= 0 new-f 7) (<= 0 new-r 7))
             (when (is-piece-at? new-idx :knight attacker board)
               (return t)))))
       
       ;; Bishop/Queen diagonal
       (ray-attack king-square '((-1 -1) (-1 1) (1 -1) (1 1)) :bishop board attacker)
       (ray-attack king-square '((-1 -1) (-1 1) (1 -1) (1 1)) :queen board attacker)
       
       ;; Rook/Queen straight
       (ray-attack king-square '((-1 0) (1 0) (0 -1) (0 1)) :rook board attacker)
       (ray-attack king-square '((-1 0) (1 0) (0 -1) (0 1)) :queen board attacker)
       
       ;; King attacks
       (dolist (move '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) nil)
         (let* ((new-f (+ (first (idx->uci king-square)) (first move)))
                (new-r (+ (second (idx->uci king-square)) (second move)))
                (new-idx (uci->idx (list new-f new-r))))
           (when (and (<= 0 new-f 7) (<= 0 new-r 7))
             (when (is-piece-at? new-idx :king attacker board)
               (return t)))))))))
