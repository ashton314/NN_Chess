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

;; High-level chunks
(feature-recognition)

(done-testing)