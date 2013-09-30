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

(let ((result1 (vector-ref (test-net00 'run '(0.35 0.9)) 0))
      (result2 (vector-ref (test-net00 'run '(0.9 0.35)) 0)))
  (ok (good result1 0.69 0.01) (format #f "test-net00 ran correctly: ~A" result1))
  (ok (not (= result1 result2)) (format #f "test-net00 returned different result correctly: ~A" result2)))

(let ((result1 (vector-ref (test-net00 'run '(0.35 0.9)) 0))
      (result2 0))
  (ok (good result1 0.69 0.01) (format #f "test-net00 ran correctly (again): ~A" result1))
  (test-net00 'train '(0.35 0.9) '(0.5))
  (set! result2 (vector-ref (test-net00 'run 0.35 0.9) 0))
  (ok (good (abs (- result2 0.5)) 0.18205 0.001) (format #f "test-net00 trained correctly: ~A" result2)))
