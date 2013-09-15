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

;; TODO: Write a sort of backwards pass through the network to set all
;; of the nodes "outputs" values

(define-syntax define-net
  (syntax-rules ()
    ((define-net net-name (input-nodes ...) (output-nodes ...) (node-name (input weight) ...) ...)
     (define (net-name op . inputs)
       (let ((nodes '((input-nodes (inputs '())
				   (outputs '())
				   (weights '())
				   (value (pop! inputs))) ...
		      (node-name (inputs input)
				 (outputs '())
				 (weights weights)
				 (value #f)) ...)))
	 (case op
	   ((run)			; inputs are in values
	    (let ((input-nodes (pop! inputs)) ...)

	      (define (get-value node)
		(let ((this-node (cdr (assoc node nodes))))
		  (let ((this-value (cadr (assoc 'value this-node))))
		    (if this-value this-value
			(let ((new-value (apply + (map * (map get-value (cadr (assoc 'inputs this-node)))
						       (cadr (assoc 'weights this-node))))))
			  (set-cdr! (assoc 'value this-node) new-value)
			  new-value)))))

	      (map (lambda (node-name)
		     
		     ) '(output-nodes ...))
	      
	      ((train)			; two lists: first are inputs, second are target values
	       
	       )