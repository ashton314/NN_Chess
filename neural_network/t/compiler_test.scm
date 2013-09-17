;;; Test for closure nodes
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../compiler.scm")

;; utility functions

(define (good val1 val2 tol)
  (< (abs (- val1 val2)) tol))

#|
(define-net test-net00
  (input1 input2)
  (out1)
  1

  ;; Hidden layers
  (node-a (input1 0.1) (input2 0.8))
  (node-b (input1 0.4) (input2 0.6))

  ;; Output nodes
  (out1 (node-a 0.3) (node-b 0.9)))

(let ((result1 (car (test-net00 'run 0.35 0.9)))
      (result2 (car (test-net00 'run 0.9 0.35))))
  (ok (good result1 0.69 0.01) (format #f "test-net00 ran correctly: ~A" result1))
  (ok (not (= result1 result2)) (format #f "test-net00 returned different result: ~A" result2)))

(let ((result1 (car (test-net00 'run 0.35 0.9)))
      (result2 0))
  (ok (good result1 0.69 0.01) (format #f "test-net00 ran correctly (again): ~A" result1))
  (test-net00 'train '(0.35 0.9) '(0.5))
  (set! result2 (car (test-net00 'run 0.35 0.9)))
  (ok (good (abs (- result2 0.5)) 0.18205 0.001) (format #f "test-net00 trained correctly: ~A" result2)))
|#

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
;	(format #t "Letter: ~A~%Last errors: ~A~%" letter (char-rec 'last-errors))
	(training (- times 1) (car letters) (reverse (cons letter (reverse (cdr letters))))))))

(training 500 (cons '(1 0 0 0) *a-bitmap*) (list (cons '(0 1 0 0) *b-bitmap*) (cons '(0 0 1 0) *c-bitmap*) (cons '(0 0 0 1) *d-bitmap*)))


(done-testing)

(write-string "Done.\n")