;;; Time trials
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../compiler.scm")
(load "../closures.scm")
(load "../matrix.scm")

(load-option 'format)

(define training-times 10000)

(define-net test-net02
  (in1 in2 in3 in4)
  (out1 out2 out3 out4)
  1

  ;; Hidden layers
  (one (in1 .1) (in2 .1) (in3 .1) (in3 .1))
  (two (in1 .1) (in2 .1) (in3 .1) (in3 .1))
  (three (in1 .1) (in2 .1) (in3 .1) (in3 .1))
  (four (in1 .1) (in2 .1) (in3 .1) (in3 .1))
  (five (in1 .1) (in2 .1) (in3 .1) (in3 .1))

  ;; Output layers
  (out1 (one .1) (two .1) (three .1) (four .1) (five .1))
  (out2 (one .1) (two .1) (three .1) (four .1) (five .1))
  (out3 (one .1) (two .1) (three .1) (four .1) (five .1))
  (out4 (one .1) (two .1) (three .1) (four .1) (five .1)))

(test-net02 'init)

(define *test02-pattern1* '(1 0 0 1))
(define *test02-pattern2* '(0 1 1 0))

(define (trainer1 times this-pattern next-pattern)
  (if (= times 0)
      #t
      (begin
	(test-net02 'train this-pattern this-pattern)
	(trainer1 (- times 1) next-pattern this-pattern))))

(with-timings
 (lambda ()
   (trainer1 training-times *test02-pattern1* *test02-pattern2*))
 (lambda (run-time gc-time real-time)
   (format #t "Compiler~%Run time : ~A~%GC Time  : ~A~%Real Time: ~A~%" run-time gc-time real-time)))


;; matrix

(define-feed-forward-net test-net01
  1
  4
  ((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
  ((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))

(define *test01-pattern0* '(1 0 0 1))
(define *test01-pattern1* '(0 1 1 0))

(define (trainer times this-pattern next-pattern)
  (if (= times 0)
      #t
      (begin
	(test-net01 'train this-pattern this-pattern)
	(trainer (- times 1) next-pattern this-pattern))))

(with-timings
 (lambda ()
   (trainer training-times *test02-pattern1* *test02-pattern2*))
 (lambda (run-time gc-time real-time)
   (format #t "Matrix~%Run time : ~A~%GC Time  : ~A~%Real Time: ~A~%" run-time gc-time real-time)))
