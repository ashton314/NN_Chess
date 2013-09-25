;;; Test for matrix feed-forward networks
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../matrix.scm")

(load-option 'format)

(define (good val1 val2 tol)
  (< (abs (- val1 val2)) tol))

(define-feed-forward-net test-net00
  1					; learning rate
  2					; num inputs
  ((0.3 0.9))				; output nodes
  ((0.1 0.8) (0.4 0.6)))		; hidden layer

(test-net00 'debug-layers)

(let ((result1 (test-net00 'run '(0.35 0.9))))
  (pretty-print result1)
  (newline)
  (format #t "Should be: ~A~%" 0.69))