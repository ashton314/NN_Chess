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

(define-syntax define-net
  (syntax-rules ()
    ((define-net net-name (input-nodes ...) (output-nodes ...) _learning-rate (node-name (input weight) ...) ...)
     (define (net-name op . input-values)
       (let ((learning-rate _learning-rate) ; prevent multiple evaluation (does this happen with Scheme's macros?)
	     (nodes `((input-nodes (sym-name input-nodes)
				   (inputs ())
				   (outputs ())
				   (weights ())
				   (error-delta 'undef)
				   (value #f)) ...
		      (node-name (sym-name node-name)
				 (inputs (input ...))
 				 (outputs ())
 				 (value #f)
				 (error-delta #f)
				 (weights (weight ...))) ...)))

	 (for-each (lambda (node)	; initilize outputs
		     (set-car! (cdr (assoc 'outputs (cdr node)))
			       (map car (filter (lambda (other-node)
						  (memq (car node) (cadr (assoc 'inputs (cdr other-node))))) nodes))))
		   nodes)

	 (define-syntax getter-setter
	   (syntax-rules ()
	     ((_ place)
	      (lambda (flag . rest)
		(if (eq? flag 'get)
		    place
		    (if (eq? flag 'set!)
			(set! place (car rest))
			(error (format #f "Unknown option passed to node: ~A -- ~A" flag loc))))))))

	 (set! nodes (map (lambda (node) ; compile pass
			    (let ((values (cdr node)))
			      (list (car node)
				    (let ((slot-sym-name (cadr (assoc 'sym-name values)))
					  (slot-inputs (cadr (assoc 'inputs values)))
					  (slot-outputs (cadr (assoc 'outputs values)))
					  (slot-weights (cadr (assoc 'weights values)))
					  (slot-error-delta (cadr (assoc 'error-delta values)))
					  (slot-value (cadr (assoc 'value values))))
				      (letrec ((this-node
						(lambda (op)
						  (case op
						    ((sym-name) (getter-setter slot-sym-name))
						    ((inputs) (getter-setter slot-inputs))
						    ((outputs) (getter-setter slot-outputs))
						    ((weights) (getter-setter slot-weights))
						    ((error-delta) (getter-setter slot-error-delta))
						    ((value)
						     (let ((data-slot (cadr (assoc 'value values))))
						       (lambda (op . rest)
							 (if (eq? op 'get-raw)
							     data-slot
							     (if (eq? op 'get)
								 (if data-slot data-slot
								     (let ((new-value (activation (apply + (map * (map (lambda (input-node)
															 ((input-node 'value) 'get))
														       ((this-node 'inputs) 'get))
														((this-node 'weights) 'get))))))
								       (set! data-slot new-value)
								       new-value))
								 (if (eq? op 'set!)
								     (set! data-slot (car rest))
								     (error (format #f "Unknown option passed to node: ~A -- value" op))))))))
						    (else (error (format #f "Unknown option to node: '~A'" op)))))))
					this-node)))))
			  nodes))

;; 	 (map (lambda (node) (pretty-print (list ((node 'sym-name) 'get)
;; 						 (list 'value ((node 'value) 'get-raw))
;; 						 (map (lambda (slot) (list slot ((node slot) 'get)))
;; 						      '(inputs outputs weights error-delta))))
;; 		      (newline))
;; 	      (map cadr nodes))


	 (for-each (lambda (node)	; linker pass
		     ((node 'inputs) 'set! (map (lambda (node) (cadr (assoc node nodes))) ((node 'inputs) 'get)))
		     ((node 'outputs) 'set! (map (lambda (node) (cadr (assoc node nodes))) ((node 'outputs) 'get))))
		   (map cadr nodes))

;; 	 (map (lambda (node) (pretty-print (list ((node 'sym-name) 'get)
;; 						 (map (lambda (slot) (list slot ((node slot) 'get)))
;; 						      '(inputs outputs weights error-delta value))))
;; 		      (newline))
;; 	      (map cadr nodes))

	 (define (back-prop node)
	   ;; node => closure-object

	   (define (searcher search-nodes search-weights)
	     ;; This takes a list of nodes and a list of weights,
	     ;; which are mapped to eachother linearly. This finds the
	     ;; weight associated with `node' (see above scope)

	     ;; OPTIMIZATION NOTE: Node-weight lookup could be
	     ;; improved through some complex mechanism in the linking
	     ;; pass
	     (if (there-exists? (list search-nodes search-weights) null?)
		 (begin
		   (debug)
		   (error (format #f "could not find node ~A in inputs of an output node!" node)))
		 (if (eq? (car search-nodes) node)
		     (car search-weights)
		     (searcher (cdr search-nodes) (cdr search-weights)))))
		 

	   (let ((bad-node (find-matching-item ((node 'outputs) 'get)
					       (lambda (out-node)
						 (not ((out-node 'error-delta) 'get))))))
	     (if bad-node ; one of this node's output nodes' error-delta has not been computed
		 (back-prop bad-node)
		 (begin
		   (if (or ((node 'error-delta) 'get)
			   (null? ((node 'inputs) 'get)))
		       #t			; already computed or not needed to correct
		       (let ((delta (let ((val ((node 'value) 'get)))
				      (* val (- 1 val)
					 (apply + (map (lambda (output-node)
							 (* ((output-node 'error-delta) 'get)
							    (searcher ((output-node 'inputs) 'get)
								      ((output-node 'weights) 'get))))
						       ((node 'outputs) 'get)))))))
			 ((node 'error-delta) 'set! delta)
			 ((node 'weights) 'set! (map (lambda (this-weight in-node)
						       (+ this-weight (* learning-rate delta ((in-node 'value) 'get))))
						     ((node 'weights) 'get)
						     ((node 'inputs) 'get)))))
						 
		   (for-each back-prop ((node 'inputs) 'get))))))

	 (case op
	   ((run)			; inputs are in values
	    (for-each (lambda (node) ((node 'value) 'set! #f)) (map cadr nodes)) ; clear memoized values
	    (for-each (lambda (node) ((node 'value) 'set! (pop! input-values))) (map (lambda (sym) (cadr (assoc sym nodes)))
										     '(input-nodes ...)))
	    (write-string "###########################################\n")
	    (net-name 'debug-nodes-pretty)
	    (write-string "###########################################\n")
	    (map (lambda (sym) (((cadr (assoc sym nodes)) 'value) 'get)) '(output-nodes ...)))

	   ((debug-nodes-pretty)
	    (map (lambda (node) (pretty-print (list ((node 'sym-name) 'get)
						    (map (lambda (slot) (list slot ((node slot) 'get)))
							 '(inputs outputs weights error-delta value))))
			 (newline))
		 (map cadr nodes)))

	   ((debug-nodes)
	    ;; For debugging purposes
	    nodes)
	      
	   ((init)
	    ;; Set node weights to random values
	    (for-each (lambda (node)
			((node 'weights) 'set!
			 (map (lambda (nul) (random-real)) ((node 'weights) 'get))))
		      (map cadr nodes)))

	   ((last-errors)
	    ;; Returns error deltas of output nodes
	    (map (lambda (node) ((node 'error-delta) 'get)) (map (lambda (sym) (cadr (assoc sym nodes))) '(output-nodes ...))))

	   ((train)
	    ;; Run backprop algorithm on network
	    ;; Inputs: (input1 input2 ...) (target1 target2 ...)
	    (for-each (lambda (node) ((node 'error-delta) 'set! #f)) (map cadr nodes)) ; clear error deltas
	    (let* ((inputs (car input-values))
		   (targets (cadr input-values))
		   (first-pass (apply net-name `(run ,@inputs)))
		   (deltas (map (lambda (output target) (* (- target output) (- 1 output) output)) first-pass targets)))
	      (for-each (lambda (node delta)
			  (let ((self-node (cadr (assoc node nodes))))
			    ((self-node 'error-delta) 'set! delta)
			    ((self-node 'weights) 'set!
			     (map (lambda (this-weight in-node)
				    (+ this-weight (* learning-rate delta ((in-node 'value) 'get))))
				  ((self-node 'weights) 'get)
				  ((self-node 'inputs) 'get)))
			    (for-each back-prop ((self-node 'inputs) 'get))))
			'(output-nodes ...) deltas)))
	   (else
	    (error "bad operation on neural net"))))))))
