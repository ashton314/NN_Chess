;;; Neural Network library, using macros
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

;; Macros

(load-option 'format)

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

;; TODO: Write a sort of backwards pass through the network to set all
;; of the nodes "outputs" values

(define-syntax define-net
  (syntax-rules ()
    ((define-net net-name (input-nodes ...) (output-nodes ...) (node-name (input weight) ...) ...)
     (define (net-name op . input-values)
       (let ((nodes `(
		      (input-nodes (inputs '())
				   (outputs '())
				   (weights '())
				   (value #f)) ...
		      (node-name (inputs (input ...))
 				 (outputs '())
 				 (value #f)
				 (weights (weight ...))) ...
				 )))

	 (for-each (lambda (node)	; Initilize outputs
		     (set-cdr! (assoc 'outputs (cdr node))
			       (map car (filter (lambda (other-node)
						  (memq (car node) (cadr (assoc 'inputs (cdr other-node))))) nodes))))
		   nodes)

	 (define (get-value node)
	   (let ((this-node (cdr (or (assoc node nodes) (error (format #f "could not find node ~A" node))))))
	     (let ((this-value (cadr (assoc 'value this-node))))
	       (if this-value this-value
		   (let ((new-value (activation (apply + (map * (map get-value (cadr (assoc 'inputs this-node)))
							      (cadr (assoc 'weights this-node)))))))
		     (set-car! (cdr (assoc 'value this-node)) new-value)
		     new-value)))))

	 (case op
	   ((run)			; inputs are in values
	    (for-each (lambda (node) (set-car! (cdr (assoc 'value (cdr node))) #f)) nodes)
	    (for-each (lambda (node) (set-car! (cdr (assoc 'value (cdr (assoc node nodes)))) (pop! input-values))) '(input-nodes ...))
	    (map get-value '(output-nodes ...)))

	   ((debug-nodes)
	    ;; For debugging purposes
	    nodes)
	      
	   ((train)			; two lists: first are inputs, second are target values
	    (error "to be implemented"))
	   (else
	    (error "bad operation on neural net"))))))))
