;;; Neural Network library, using closures
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


;; Variables

(define *bias* 1)
(define *learning-rate* 1)
(define *true-signal* 1)
(define *false-signal* 0)

(define (activation value)
  (/ 1 (+ 1 (exp (- value)))))

(define (make-constant-node value)
  (let ((node (make-node '(weights (1)))))
    ((node 'value) 'set! value)
    (lambda (operation)
      (case operation
	((initilize-weights calculate-error learn back-prop reset) #f) ; forbidden actions
	((run) ((node 'value) 'get))
	(else (node operation))))))

(define (make-node . data)

  (define-syntax do-it
    ;; This is just to make getting default data into the closure
    ;; variables prettier and easier.
    (syntax-rules ()
      ((do-it slot default)
       (or (let ((temp (assoc 'slot data)))
	     (if temp
		 (cadr temp)
		 default))))))

  (define-syntax getter-setter
    (syntax-rules ()
      ((getter-setter variable)
       (lambda (op . rest)
	 (if (eq? op 'get) variable
	     (if (eq? op 'set!) (set! variable (car rest))
		 (error (format #f "Unknown option passed to node: ~A -- ~A" op 'variable))))))))

  (let* ((input-nodes  (do-it input-nodes '()))
	 (weights      (do-it weights '()))
	 (bias         (do-it bias *bias*))
	 (output       (do-it output #f))
	 (output-nodes (do-it output-nodes '()))
	 (function     (do-it function (lambda (inputs weights)
					 (apply + (map * inputs weights)))))
	 (error-delta #f)
	 (value #f))

    (define self (lambda (operation)			; I need to refer to this node within itself for the init function
		   (case operation			; This could be a symbol or a list of values as inputs
		     ((init)
		      ;; This sends itself to all the nodes that it gets output from for backprop
		      (map (lambda (node) ((node 'push-outputs) self)) input-nodes))
		     
		     ((push-outputs)
		      ;; takes a node closure and pushes it onto output-nodes
		      (lambda (node) (push! node output-nodes)))

		     ((output-nodes)
		      (getter-setter output-nodes))
		     
		     ((input-nodes)
		      (getter-setter input-nodes))
		     
		     ((weights)
		      (getter-setter weights))
		     
		     ((initilize-weights)
		      ;; set weights to random values
		      (set! weights (map (lambda (nul) (random-real)) input-nodes)))
		     
		     ((output)
		      (getter-setter output))
		     
		     ((bias)
		      (getter-setter bias))
		     
		     ((function)
		      (getter-setter function))
		     
		     ((value)
		      (getter-setter value))
		     
		     ((error-delta)
		      (lambda (op . rest)
			(if (eq? op 'get)
			    (if error-delta
				error-delta
				(begin
				  (self 'back-prop)
				  error-delta))
			    (if (eq? op 'set!)
				(set! error-delta (car rest))
				(error (format #f "Unknown option passed to node: ~A -- error-delta" op))))))
		     
		     ((reset)
		      ;; reset value to #f
		      (set! value #f) (map (lambda (node) (node 'reset)) input-nodes))
		     
		     ((run)
		      ;; probe the node (recursively calls any input nodes) and
		      ;; memotize the value for the node
		      (if value value
			  (let* ((val (function (map (lambda (node) (node 'run)) input-nodes) weights))
				 (out (activation val)))
			    (set! value out)
			    (if output (output out) out))))
		     
		     
		     ;; Learning routines
		     
		     ((calculate-error)
		      ;; This is to be called ONLY on output nodes
		      ;; Must call on ALL the output nodes before running learn!!
		      (lambda (target)
			(let* ((out (activation (function (map (lambda (node) (node 'run)) input-nodes) weights)))
			       (d-a (* out (- 1 out) (- target out))))
			  (set! error-delta d-a)
			  (format #t "Error for output node is: ~A~%" error-delta)
			  (set! weights (map (lambda (weight node) (+ weight (* *learning-rate* d-a ((node 'value) 'get))))
					     weights input-nodes)))))
		     
		     ((learn)
		      ;; Must call calculate-error on all output nodes BEFORE running this function
		      ;; This function should be called ONCE (I think)
		      (map (lambda (node) (node 'back-prop)) input-nodes))
		     
		     ((back-prop)
		      ;; Grabs deltas from downstream nodes and uses it to calculate the deltas for its weights
		      (letrec ((finder (lambda (key key-list value-list)
					 ;; this is so I can find the weight of this
					 ;; node given the list of input nodes and
					 ;; weights of the downstream node
					 (if (or (null? key-list) (null? value-list))
					     (error "Could not find key in list!")
					     (if (eq? key (car key-list))
						 (car value-list)
						 (finder key (cdr key-list) (cdr value-list)))))))

			(if (null? output-nodes) (error "No output nodes defined for this node!"))
			
			(let ((delta (* value (- 1 value)
					(apply + (map (lambda (downstream-node)
							(* ((downstream-node 'error-delta) 'get) ; Downstream error
							   (finder self ((downstream-node 'input-nodes) 'get)
								   ((downstream-node 'weights) 'get))))
						      output-nodes)))))
			  (format #t "Error delta is: ~A~%" delta)
			  (set! error-delta delta)
			  (set! weights (map (lambda (weight node) (+ weight (* *learning-rate* delta ((node 'value) 'get))))
					     weights input-nodes)))
			(map (lambda (node) (node 'back-prop)) input-nodes)))
		     
		     
		     (else (if (or (not (pair? operation)) (not (= (length operation) (length weights))))
			       (error "Wrong input to neural node!")
			       (if (>= (function operation weights) bias)
				   *true-signal*
				   *false-signal*))))))
    self))
