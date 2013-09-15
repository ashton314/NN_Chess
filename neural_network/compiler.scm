;;; Neural Network library, using macros
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

;; Macros

(define-syntax push!
  (syntax-rules ()
    ((push! datum place)
     (set! place (cons datum place)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! place)
     (let ((temp1 (car place)))
       (set! place (cdr place))
       temp1))))


(define (activation value)
  ;; Sigmoid function
  (/ 1 (+ 1 (exp (- value)))))

(define-net test-net
  (input1 input2)
  (output1 output2)

  ;; Hidden layers
  (node-a (input1 0.1) (input2 0.8))
  (node-b (input1 0.4) (input2 0.6))

  ;; Output nodes
  (out1 (node-a 0.3) (node-b 0.9))
  (out2 (node-a 0.1) (node-b 0.5)))

;; Root learning call
(test-net 'train target1 target2 ...)

(define-syntax create-nodes
  (syntax-rules ()
    ((create-nodes ((node-name (input weight) ...) ...) logic)
     (let ((node-name '((inputs input ...)
			(weights weight ...)
			(value #f))) ...)
       logic))))

(define-syntax define-net
  (syntax-rules ()
    ((define-net net-name (input-nodes ...) (output-nodes ...) node-descriptors ...)
     (define (net-name op . inputs)
       (create-nodes (node-descriptors ...)
	  (case op
	    ((run)			; inputs are in values
	     (let ((input-nodes (pop! inputs)) ...)
	       (map (lambda (node-name)
		      
		      ) output-nodes ...)

	    ((train)			; two lists: first are inputs, second are target values

	     )