;;; Time trials
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../compiler.scm")
(load "../closures.scm")

(load-option 'format)

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

(let* ((pass1 (test-net02 'train *test02-pattern1* *test02-pattern1*))
       (error-pass1 (apply + (map abs (test-net02 'last-errors))))
       (pass2 (test-net02 'train *test02-pattern2* *test02-pattern2*))
       (error-pass2 (apply + (map abs (test-net02 'last-errors))))
       (training-times 1000))
  (format #t "Pass1: ~A  Error: ~A~%Pass2: ~A  Error: ~A~%" pass1 error-pass1 pass2 error-pass2)
  (define (trainer times this-pattern next-pattern last-error1 last-error2)
    (if (= times 0)
	#t
	(begin
	  (test-net02 'train this-pattern this-pattern)
	  (let ((err (apply + (map abs (test-net02 'last-errors)))))
	    (if (> err last-error1)
		(format #t "Warn! Error increasing (Error: ~A) after ~A iterations!~%" err (- training-times times)))
	    (trainer (- times 1) next-pattern this-pattern last-error2 err)))))
  (with-timings
   (lambda ()
     (trainer training-times *test02-pattern1* *test02-pattern2* error-pass1 error-pass2))
   (lambda (run-time gc-time real-time)
     (format #t "Run time : ~A~%GC Time  : ~A~%Real Time: ~A~%" run-time gc-time real-time))))
