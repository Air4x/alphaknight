(asdf:load-asd "alphaknight.asd")
(ql:quickload "alphaknight")
(setf uiop:*image-entry-point* #'alphaknight:start)
(uiop:dump-image "alphaknight" :executable t :compression 9)
