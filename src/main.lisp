(in-package :alphaknight)

(defvar *position* (make-Posizione))
(init-Posizione *position*)

(defun start ()
  (format t "Welcome to Alphaknight, the simulation will begin...~%")
  (loop
   (print-board (Posizione-board *position*))
   (let ((best-move (find-best-move *position* 3)))
     (setf *position* (apply-move *position* best-move))
     (print-board (Posizione-board *position*))
     (terpri))))
