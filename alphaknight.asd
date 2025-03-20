(asdf:defsystem #:alphaknight
    :description "Semplice chess engine utilizzando Minimax ed alpha-beta pruning"
    :author "Mario Calcagno"
    :license "GPL"
    :version "0.1"
    :serial t
    :components ((:file "src/package")
		 (:file "src/board")
		 (:file "src/main")))
