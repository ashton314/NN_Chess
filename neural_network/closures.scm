;;; Neural Network library, using closures
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(define *bias* 1)
(define *learning-rate* 1)
(define *true-signal* 1)
(define *false-signal* 0)

(define (activation value)
  (/ 1 (+ 1 (exp (- value)))))

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

  (let* ((input-nodes  (do-it input-nodes '()))
	 (weights      (do-it weights '()))
	 (bias         (do-it bias *bias*))
	 (output       (do-it output #f))
	 (function     (do-it function (lambda (inputs weights)
					 (apply + (map * inputs weights)))))
	 (value #f))

    (lambda (operation)
      (case operation			; This could be a symbol or a list of values as inputs
	((input-nodes)
	 ;; getter-setter for input node functions
	 (lambda (op . rest) (if (eq? op 'get) input-nodes
				 (if (eq? op 'set!) (set! input-nodes (car rest)) (error "Unknown option -- input-nodes")))))

	((weights)
	 ;; getter-setter for weights
	 (lambda (op . rest) (if (eq? op 'get) weights
				 (if (eq? op 'set!) (set! weights (car rest)) (error "Unknown option -- weights")))))

	((initilize-weights)
	 ;; set weights to random values
	 (set! weights (map (lambda (nul) (random-real)) input-nodes)))

	((output)
	 ;; getter-setter for output location
	 (lambda (op . rest) (if (eq? op 'get) output
				 (if (eq? op 'set!) (set! output (car rest)) (error "Unknown option -- output")))))

	((bias)
	 ;; getter-setter for bias
	 (lambda (op . rest) (if (eq? op 'get) bias
				 (if (eq? op 'set!) (set! bias (car rest)) (error "Unknown option -- bias")))))

	((function)
	 ;; getter-setter for summation function
	 (lambda (op . rest) (if (eq? op 'get) function
				 (if (eq? op 'set!) (set! function (car rest)) (error "Unknown option -- function")))))

	((value)
	 ;; getter-setter for value
	 (lambda (op . rest) (if (eq? op 'get) value
				 (if (eq? op 'set!) (set! value (car rest)) (error "Unknown option -- value")))))

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

	((learn)
	 ;; Calls back-prop on input nodes. This is to be called ONLY on output nodes
	 (lambda (target)
	   (let* ((out (activation (function (map (lambda (node) (node 'run)) input-nodes))))
		  (d-a (* out (- 1 out) (- target out))))
	     (set! weights (map (lambda (weight node) (+ weight (* *learning-rate* d-a ((node 'value) 'get)))) weights input-nodes)))))
	     

	((back-prop)
	 (lambda (


	(else (if (or (not (pair? operation)) (not (= (length operation) (length weights))))
		  (error "Wrong input to neural node!")
		  (if (>= (function operation weights) bias)
		      *true-signal*
		      *false-signal*)))))))
