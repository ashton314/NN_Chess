;;; Checkers neural network test -*- mode: scheme -*-
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load "../../lib/tap.scm")
(load "brain.scm")

(define (feature-recognition)
  (let ((board #(#(0 0 0 0) #(0 0 -2 -1) #(0 0 0 1) #(1 -1 0 -1) #(0 0 -1 0) #(-1 -1 1 2) #(0 -1 0 0) #(-1 -1 0 0))))
    (is (pawn-compare board) -6 "pawn comparison works for board0")
    (is (king-compare board) 0 "king comparison works for board0")
    (is (white-advancement-potential board) 1 "white advancement potential works for board0")
    (is (black-advancement-potential board) 1 "black advancement potential works for board0")
    (let ((results (analyze-movement board)))
      (is (car results) 6 "white has 6 moves in board0")
      (is (cadr results) 15 "black has 15 moves in board0")
      (is (caddr results) 0 "white has no endangered pieces")
      (is (cadddr results) 1 "black has one endangered piece"))

    (set! board #(#(0 0 0 0) #(1 0 0 1) #(-1 0 -2 1) #(1 0 -1 0) #(1 0 -1 -1) #(-1 -1 0 0) #(-1 -1 2 0) #(-1 -1 0 0)))
    (is (pawn-compare board) -5 "pawn comparison works for board1")
    (is (king-compare board) 0 "king comparison works for board1")
    (is (white-advancement-potential board) 0 "white advancement potential works for board1")
    (is (black-advancement-potential board) 1 "black advancement potential works for board1")
    (let ((results (analyze-movement board)))
      (is (car results) 6 "white has 6 moves in board1")
      (is (cadr results) 10 "black has 10 moves in board1")
      (is (caddr results) 2 "white has 2 endangered pieces")
      (is (cadddr results) 1 "black has one endangered piece"))

    (set! board #(#(0 0 0 0) #(1 0 -2 1) #(0 0 1 1) #(1 1 -1 1) #(1 -1 0 -1) #(-1 -1 0 -1) #(-1 -1 -1 0) #(-1 -1 0 0)))
    (is (pawn-compare board) -3 "pawn comparison works for board2")
    (is (king-compare board) -1 "king comparison works for board2")
    (is (white-advancement-potential board) 0 "white advancement potential works for board2")
    (is (black-advancement-potential board) 0 "black advancement potential works for board2")
    (let ((results (analyze-movement board)))
      (is (car results) 4 "white has 4 moves in board2")
      (is (cadr results) 8 "black has 8 moves in board2")
      (is (caddr results) 1 "white has one endangered piece")
      (is (cadddr results) 2 "black has two endangered pieces"))))


;; High-level chunks
(feature-recognition)

(done-testing)