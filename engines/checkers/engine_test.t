;;; Checkers Engine Test   -*- mode: scheme -*-
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load "../../lib/tap.scm")
(load "engine.scm")

;; Test routines
(define (board-displays)
  (let ((board (make-board)))
    (board 'print)
    (ok (prompt-for-confirmation "Does this look right?") "board printing works")))

;; High-level chunks
(board-displays)

(done-testing)