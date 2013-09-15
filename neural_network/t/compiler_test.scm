;;; Test for closure nodes
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../compiler.scm")

;; utility functions

(define (good val1 val2 tol)
  (< (abs (- val1 val2)) tol))

(define-net test-net00
  (input1 input2)
  (out1)

  ;; Hidden layers
  (node-a (input1 0.1) (input2 0.8))
  (node-b (input1 0.4) (input2 0.6))

  ;; Output nodes
  (out1 (node-a 0.3) (node-b 0.9))
  (out2 (node-a 0.1) (node-b 0.5)))

(let ((result1 (car (test-net00 'run 0.35 0.9)))
      (result2 (car (test-net00 'run 0.9 0.35))))
  (ok (good result1 0.69 0.01) (format #f "test-net00 ran correctly: ~A" result1))
  (ok (not (= result1 result2)) (format #f "test-net00 returned different result: ~A" result2)))

(done-testing)

(write-string "Done.\n")