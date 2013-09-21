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
  1

  ;; Hidden layers
  (node-a (input1 0.1) (input2 0.8))
  (node-b (input1 0.4) (input2 0.6))

  ;; Output nodes
  (out1 (node-a 0.3) (node-b 0.9)))

(test-net00 'debug-nodes-pretty)

(let ((result1 (car (test-net00 'run 0.35 0.9)))
      (result2 (car (test-net00 'run 0.9 0.35))))
  (ok (good result1 0.69 0.01) (format #f "test-net00 ran correctly: ~A" result1))
  (ok (not (= result1 result2)) (format #f "test-net00 returned different result: ~A" result2)))

(test-net00 'debug-nodes-pretty)

(let ((result1 (car (test-net00 'run 0.35 0.9)))
      (result2 0))
  (ok (good result1 0.69 0.01) (format #f "test-net00 ran correctly (again): ~A" result1))
  (test-net00 'train '(0.35 0.9) '(0.5))
  (set! result2 (car (test-net00 'run 0.35 0.9)))
  (ok (good (abs (- result2 0.5)) 0.18205 0.001) (format #f "test-net00 trained correctly: ~A" result2)))

(test-net00 'debug-nodes-pretty)


(define-net test-net01
  (in1 in2)
  (out1 out2)
  1

  ;; Hidden layers
  (one (in1 .1) (in2 .1))
  (two (in1 .1) (in2 .1))

  ;; Output layers
  (out1 (one .1) (two .1))
  (out2 (one .1) (two .1)))

(test-net01 'init)

(define *test01-pattern1* '(1 0))
(define *test01-pattern2* '(0 1))

(let* ((pass1 (test-net01 'train *test01-pattern1* *test01-pattern1*))
       (error-pass1 (apply + (map abs (test-net01 'last-errors))))
       (pass2 (test-net01 'train *test01-pattern2* *test01-pattern2*))
       (error-pass2 (apply + (map abs (test-net01 'last-errors))))
       (training-times 1000))
  (format #t "Pass1: ~A  Error: ~A~%Pass2: ~A  Error: ~A~%" pass1 error-pass1 pass2 error-pass2)
  (define (trainer times this-pattern next-pattern last-error1 last-error2)
    (if (= times 0)
	#t
	(begin
	  (test-net01 'train this-pattern this-pattern)
	  (let ((err (apply + (map abs (test-net01 'last-errors)))))
	    (if (> err last-error1)
		(format #t "Warn! Error increasing (Error: ~A) after ~A iterations!~%" err (- training-times times)))
	    (trainer (- times 1) next-pattern this-pattern last-error2 err)))))
  (trainer training-times *test01-pattern1* *test01-pattern2* error-pass1 error-pass2))

(write-string "\n\n\n")

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
  (trainer training-times *test02-pattern1* *test02-pattern2* error-pass1 error-pass2))

(done-testing)

(write-string "Done.\n")

#|
;; Character recognition
(define-net char-rec
  (a0 a1 a2 a3 a4
   b0 b1 b2 b3 b4
   c0 c1 c2 c3 c4
   d0 d1 d2 d3 d4
   e0 e1 e2 e3 e4
   f0 f1 f2 f3 f4
   g0 g1 g2 g3 g4)
  (is-a is-b is-c is-d)
  1

  ;; Hidden layers
  (node1 (a0 .1) (a1 .1) (a2 .1) (a3 .1) (a4 .1) (b0 .1) (b1 .1) (b2 .1) (b3 .1) (b4 .1)
	 (c0 .1) (c1 .1) (c2 .1) (c3 .1) (c4 .1) (d0 .1) (d1 .1) (d2 .1) (d3 .1) (d4 .1)
	 (e0 .1) (e1 .1) (e2 .1) (e3 .1) (e4 .1) (f0 .1) (f1 .1) (f2 .1) (f3 .1) (f4 .1)
	 (g0 .1) (g1 .1) (g2 .1) (g3 .1) (g4 .1))
  (node2 (a0 .1) (a1 .1) (a2 .1) (a3 .1) (a4 .1) (b0 .1) (b1 .1) (b2 .1) (b3 .1) (b4 .1)
	 (c0 .1) (c1 .1) (c2 .1) (c3 .1) (c4 .1) (d0 .1) (d1 .1) (d2 .1) (d3 .1) (d4 .1)
	 (e0 .1) (e1 .1) (e2 .1) (e3 .1) (e4 .1) (f0 .1) (f1 .1) (f2 .1) (f3 .1) (f4 .1)
	 (g0 .1) (g1 .1) (g2 .1) (g3 .1) (g4 .1))
  (node3 (a0 .1) (a1 .1) (a2 .1) (a3 .1) (a4 .1) (b0 .1) (b1 .1) (b2 .1) (b3 .1) (b4 .1)
	 (c0 .1) (c1 .1) (c2 .1) (c3 .1) (c4 .1) (d0 .1) (d1 .1) (d2 .1) (d3 .1) (d4 .1)
	 (e0 .1) (e1 .1) (e2 .1) (e3 .1) (e4 .1) (f0 .1) (f1 .1) (f2 .1) (f3 .1) (f4 .1)
	 (g0 .1) (g1 .1) (g2 .1) (g3 .1) (g4 .1))
  (node4 (a0 .1) (a1 .1) (a2 .1) (a3 .1) (a4 .1) (b0 .1) (b1 .1) (b2 .1) (b3 .1) (b4 .1)
	 (c0 .1) (c1 .1) (c2 .1) (c3 .1) (c4 .1) (d0 .1) (d1 .1) (d2 .1) (d3 .1) (d4 .1)
	 (e0 .1) (e1 .1) (e2 .1) (e3 .1) (e4 .1) (f0 .1) (f1 .1) (f2 .1) (f3 .1) (f4 .1)
	 (g0 .1) (g1 .1) (g2 .1) (g3 .1) (g4 .1))
  (node5 (a0 .1) (a1 .1) (a2 .1) (a3 .1) (a4 .1) (b0 .1) (b1 .1) (b2 .1) (b3 .1) (b4 .1)
	 (c0 .1) (c1 .1) (c2 .1) (c3 .1) (c4 .1) (d0 .1) (d1 .1) (d2 .1) (d3 .1) (d4 .1)
	 (e0 .1) (e1 .1) (e2 .1) (e3 .1) (e4 .1) (f0 .1) (f1 .1) (f2 .1) (f3 .1) (f4 .1)
	 (g0 .1) (g1 .1) (g2 .1) (g3 .1) (g4 .1))
  (node6 (a0 .1) (a1 .1) (a2 .1) (a3 .1) (a4 .1) (b0 .1) (b1 .1) (b2 .1) (b3 .1) (b4 .1)
	 (c0 .1) (c1 .1) (c2 .1) (c3 .1) (c4 .1) (d0 .1) (d1 .1) (d2 .1) (d3 .1) (d4 .1)
	 (e0 .1) (e1 .1) (e2 .1) (e3 .1) (e4 .1) (f0 .1) (f1 .1) (f2 .1) (f3 .1) (f4 .1)
	 (g0 .1) (g1 .1) (g2 .1) (g3 .1) (g4 .1))

  ;; Output layer
  (is-a (node1 .1) (node2 .1) (node3 .1) (node4 .1) (node5 .1) (node6 .1))
  (is-b (node1 .1) (node2 .1) (node3 .1) (node4 .1) (node5 .1) (node6 .1))
  (is-c (node1 .1) (node2 .1) (node3 .1) (node4 .1) (node5 .1) (node6 .1))
  (is-d (node1 .1) (node2 .1) (node3 .1) (node4 .1) (node5 .1) (node6 .1)))

(char-rec 'init)

(map (lambda (o) (pretty-print o) (write-string "\n")) (char-rec 'debug-nodes))

; training phase
(define *a-bitmap* '(0 0 1 0 0 0 1 0 1 0 1 0 0 0 1 1 1 1 1 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1))
(define *b-bitmap* '(1 1 1 1 0 1 0 0 0 1 1 0 0 0 1 1 1 1 1 0 1 0 0 0 1 1 0 0 0 1 1 1 1 1 0))
(define *c-bitmap* '(1 1 1 1 1 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 1 1 1 1))
(define *d-bitmap* '(1 1 1 1 0 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1 1 1 1 0))

(define (training times letter letters)
  (if (= times 0)
      #t				; halt
      (begin
	(char-rec 'train (cdr letter) (car letter))
; 	(format #t "Letter: ~A~%Last errors: ~A~%" letter (char-rec 'last-errors))
	(training (- times 1) (car letters) (reverse (cons letter (reverse (cdr letters))))))))

(training 100 (cons '(1 0 0 0) *a-bitmap*) (list (cons '(0 1 0 0) *b-bitmap*) (cons '(0 0 1 0) *c-bitmap*) (cons '(0 0 0 1) *d-bitmap*)))

|#
