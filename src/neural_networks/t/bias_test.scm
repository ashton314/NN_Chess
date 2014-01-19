;;; Test for ffanns with bias inputs
;;; Ashton Wiersdorf

(load "../../utils/tap.scm")
(load "../neural_network.scm")

(set! *debug* #t)

(define *pat0* '(1 0 0 1))
(define *pat1* '(0 1 1 0))
(define test-net (define-ffn .7 4 2 4))

(test-net 'x-ray)
(test-net 'run *pat0*)
(test-net 'x-ray)

(define (trainer network times this-pattern target1 next-pattern target2)
  (if (= times 0)
      #t
      (begin
	(network 'train this-pattern target1)
	(trainer network (- times 1) next-pattern target2 this-pattern target1))))

(define (train times)
  (trainer test-net times *pat0* '(1 0) *pat1* '(0 1)))

(done-testing)
