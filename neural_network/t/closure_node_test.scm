;;; Test for closure nodes
;;; Ashton Wiersdorf

(load "../../lib/tap.scm")
(load "../closures.scm")

(plan-tests 10)

(define *input0-input* 1)
(define *input1-input* .5)

(define input0 (make-node))

(is ((input0 'input-nodes) 'get) '() "input nodes set to '() for input0")
(is ((input0 'weights) 'get) '() "weights set to '() for input0")
(is ((input0 'bias) 'get) *bias* "bias set to default for input0")

((input0 'input-nodes) 'set! (lambda (op) *input0-input*))
((input0 'weights) 'set! '(.5))

(define input1 (make-node))

(is ((input1 'input-nodes) 'get) '() "input nodes set to '() for input1")
(is ((input1 'weights) 'get) '() "weights set to '() for input1")
(is ((input1 'bias) 'get) *bias* "bias set to default for input1")

((input1 'input-nodes) 'set! (lambda (op) *input1-input*))
((input1 'weights) 'set! '(.5))

(define output (make-node `((input-nodes (,input0 ,input1))
			    (weights (.5 .5)))))
(define result #f)
((output 'output) 'set! (lambda (val) (set! result #f) 'from-result))

(is ((output 'input-nodes) 'get) (list input0 input1) "input nodes correct for output")
(is ((output 'weights) 'get) (list .5 .5) "weights correct for output node")

(is-eq (output 'run) 'from-result "output.output returned correctly")
(ok result "result is set")


(done-testing)