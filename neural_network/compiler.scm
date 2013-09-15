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
    ((define-net net-name (input-nodes ...) (output-nodes ...) _learning-rate (node-name (input weight) ...) ...)
     (define (net-name op . input-values)
       (let ((learning-rate _learning-rate) ; prevent multiple evaluation (does this happen with Scheme's macros)
	     (nodes `((input-nodes (inputs '())
				   (outputs '())
				   (weights '())
				   (error-delta 'undef)
				   (value #f)) ...
		      (node-name (inputs (input ...))
 				 (outputs '())
 				 (value #f)
				 (error-delta #f)
				 (weights (weight ...))) ...)))

	 (for-each (lambda (node)	; initilize outputs
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

	 (define (back-prop this-node node)
	   ;; `node' is the cdr of (assoc ,node-name nodes)

	   (define (searcher nodes weights)
	     (if (there-exists? null? (list nodes weights))
		 (error (format #f "could not find node ~A in inputs of an output node!" this-node))
		 (if (eq? (car nodes) this-node)
		     (car weights)
		     (searcher (cdr nodes) (cdr weights)))))
		 

	   (if (cadr (assoc 'error-delta node))
	       #t			; already computed
	       (let ((delta (let ((val (cadr (assoc 'value node))))
			      (* val (- 1 val)
				 (apply + (map (lambda (output-node) (* (cadr (assoc 'error-delta output-node))
									(searcher (cadr (assoc 'input-nodes output-node))
										  (cadr (assoc 'weights output-node)))))
					       (map (lambda (sym) (cdr (assoc sym nodes))) (cadr (assoc 'output-nodes node)))))))))
		 (set-car! (cdr (assoc 'error-delta node)) delta)
		 (set-car! (cdr (assoc 'weights node))
			   (map (lambda (weight in-node)
				  (+ weight (* learning-rate delta (cadr (assoc 'value in-node)))))
				(map (lambda (sym) (cdr (assoc sym nodes))) (cadr (assoc 'input-nodes node)))))))
						 
	   (for-each (lambda (input-node) (back-prop input-node (cdr (assoc input-node nodes)))) ; recurse to input nodes
		     (cadr (assoc 'input-nodes node))))

	 (case op
	   ((run)			; inputs are in values
	    (for-each (lambda (node) (set-car! (cdr (assoc 'value (cdr node))) #f)) nodes) ; clear memoized values
	    (for-each (lambda (node) (set-car! (cdr (assoc 'value (cdr (assoc node nodes)))) (pop! input-values))) '(input-nodes ...))
	    (map get-value '(output-nodes ...)))

	   ((debug-nodes)
	    ;; For debugging purposes
	    nodes)
	      
	   ((train)			; two lists: first are inputs, second are target values
	    (for-each (lambda (node) (set-car! (cdr (assoc 'error-delta (cdr node))) #f)) nodes) ; clear error deltas
	    (let* ((inputs (car input-values))
		   (targets (cadr input-values))
		   (first-pass (apply net-name `(run ,@inputs)))
		   (deltas (map (lambda (output target) (* (- target output) (- 1 output) output)) first-pass targets)))
	      (for-each (lambda (node delta)
			  (let ((self-node (cdr (assoc node nodes))))
			    (set-car! (cdr (assoc 'error-delta self-node)) delta)
			    (set-car! (cdr (assoc 'weights self-node))
				      (map (lambda (weight in-node)
					     (+ weight (* learning-rate delta (cadr (assoc value (cdr (assoc in-node nodes)))))))
					   (cdr (assoc 'weights self-node))
					   (cdr (assoc 'input-nodes self-node))))
			    (for-each (lambda (input-node) (back-prop input-node (cdr (assoc input-node nodes))))
				      (cadr (assoc 'input-nodes self-node)))))
			'(output-nodes ...) deltas)))
	   (else
	    (error "bad operation on neural net"))))))))
