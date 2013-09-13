;;; Test for closure nodes
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../closures.scm")

;; utility functions

(define (good val1 val2 tol)
  (< (abs (- val1 val2)) tol))

;; (plan-tests 11)

(define *input0-input* 1)
(define *input1-input* 1)

(define *s0-weight* 1)
(define *s1-weight* 1)
(define *0o-weight* .5)
(define *1o-weight* .5)

(define input0 (make-node))

(is ((input0 'input-nodes) 'get) '() "input nodes set to '() for input0")
(is ((input0 'weights) 'get) '() "weights set to '() for input0")
(is ((input0 'bias) 'get) *bias* "bias set to default for input0")

((input0 'input-nodes) 'set! (list (lambda (op) *input0-input*)))

(input0 'initilize-weights)
(is (length ((input0 'weights) 'get)) 1 "correct number of weights initilized for input0")
(ok (and (> (car ((input0 'weights) 'get)) 0) (< (car ((input0 'weights) 'get)) 1)) (format #f "weight set between 0 and 1 for input0: ~A"
											    (car ((input0 'weights) 'get))))

((input0 'weights) 'set! (list *s0-weight*))
(is ((input0 'weights) 'get) (list *s0-weight*) "weights set correctlly for input0")

(define input1 (make-node))

(is ((input1 'input-nodes) 'get) '() "input nodes set to '() for input1")
(is ((input1 'weights) 'get) '() "weights set to '() for input1")
(is ((input1 'bias) 'get) *bias* "bias set to default for input1")

((input1 'input-nodes) 'set! (list (lambda (op) *input1-input*)))
((input1 'weights) 'set! (list *s1-weight*))

(define output (make-node `(input-nodes (,input0 ,input1)) `(weights (,*0o-weight* ,*1o-weight*))))
(define result #f)
((output 'output) 'set! (lambda (val) (set! result val) 'from-result))

(is ((output 'input-nodes) 'get) (list input0 input1) "input nodes correct for output")
(is ((output 'weights) 'get) (list *0o-weight* *1o-weight*) "weights correct for output node")

(is-eq (output 'run) 'from-result "output.output returned correctly")
(ok result "result is set")

;; (write-string "Result: ")
;; (pretty-print result)
;; (write-string "\n")

;;; Single node testing

(define test-node0 (make-node `(weights (.5 .2 .1)) (list 'input-nodes
							  (list (lambda (op) .1) (lambda (op) .5) (lambda (op) .3)))))

(is ((test-node0 'weights) 'get) '(.5 .2 .1) "weights set correctly for test-node0")
(define test0-result (test-node0 'run))
(ok (< (abs (- test0-result .545)) .001) (format #f "node computed value correctly: ~A" test0-result))

;;; Two-layer network testing (no learning)
 ;; Side note: This might actually be a three-layer network...

(define *test1-input0* 0.2)
(define *test1-input1* 0.4)

(define test1-input0 (make-node '(weights (.1 .9)) `(input-nodes (,(lambda (op) *test1-input0*)
								  ,(lambda (op) *test1-input1*)))))
(define test1-input1 (make-node '(weights (.5 .8)) `(input-nodes (,(lambda (op) *test1-input0*)
								  ,(lambda (op) *test1-input1*)))))
(define test1-output (make-node '(weights (.7 .5)) `(input-nodes (,test1-input0 ,test1-input1))))

(define test1-result (test1-output 'run))
(ok (good test1-result 0.672 0.001) (format #f "network returned correctly: ~A" test1-result))


;;; Three-layer network testing (with learning)

(define *test2-inputA* (make-constant-node 0.35))
(define *test2-inputB* (make-constant-node 0.9))
(define *test2-target* 0.5)

(define test2-hidden0 (make-node '(weights (.1 .8)) `(input-nodes (,*test2-inputA*
								   ,*test2-inputB*))))
(define test2-hidden1 (make-node '(weights (.4 .6)) `(input-nodes (,*test2-inputA*
								   ,*test2-inputB*))))

(define test2-output (make-node '(weights (.3 .9)) `(input-nodes (,test2-hidden0 ,test2-hidden1))))

;; (test2-hidden0 'init)
;; (test2-hidden1 'init)
(test2-output 'init)
(ok (not (null? ((test2-hidden0 'output-nodes) 'get))) "output nodes defined for hidden node 0")
(ok (not (null? ((test2-hidden1 'output-nodes) 'get))) "output nodes defined for hidden node 1")

(define test2-result0 (test2-output 'run))
(ok (good test2-result0 0.69 0.01) (format #f "learning network ran correctly (pass 1): ~A" test2-result0))

((test2-output 'calculate-error) *test2-target*)

(ok (good ((test2-output 'error-delta) 'get) -0.0406 0.001)
    (format #f "error for output node correct: ~A" ((test2-output 'error-delta) 'get)))

(test2-output 'learn)
(test2-output 'reset)

(define test2-result1 (test2-output 'run))
(ok (good (abs (- test2-result1 0.5)) 0.18205 0.001) (format #f "network learned correctly: ~A" test2-result1))

(done-testing)

(do ((i 0 (+ i 1)))
    ((> i 1000) (format #t "Final result: ~A~%" (test2-output 'run)))
  ((test2-output 'calculate-error) *test2-target*)
  (test2-output 'learn)
  (test2-output 'reset)
  (test2-output 'run))

(write-string "Done.\n")