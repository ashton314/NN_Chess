;;; Test for 3-layer ffnns
;;; Ashton Wiersdorf

(load "../../utils/tap.scm")
(load "../neural_network.scm")

(load-option 'format)

(define (good val1 val2 tol)
  (< (abs (- val1 val2)) tol))


(define *test-net* (define-ffn 1 2 '((-.8 .1 -.2) (.2 0 -.9))
		     '((.4 .4) (.6 -.4) (-.5 .2))
		     '((-.6 -.3 .9) (.5 -.7 .6) (-.7 .8 -.4))))

(let ((results (*test-net* 'run '(0 1))))
  (ok (good (vector-ref results 0) .394 .1)
      "first output good")
  (ok (good (vector-ref results 1) .431 .1)
      "second output goood")
  (*test-net* 'train '(0 1) '(1 0)))

(done-testing)
