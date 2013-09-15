;;; Test for closure nodes
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../compiler.scm")

;; utility functions

(define (good val1 val2 tol)
  (< (abs (- val1 val2)) tol))



(done-testing)

(write-string "Done.\n")