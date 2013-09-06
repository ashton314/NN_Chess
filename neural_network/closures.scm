;;; Neural Network library, using closures
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(define *bias* 1)
(define *learning-rate* 0.01)
(define *true-signal* 1)
(define *false-signal* 0)

(define (make-node . data)
  (let ((input-nodes  (or (assoc 'input-nodes data) '()))
	(weights      (or (assoc 'weights data) '()))
	(bias         (or (assoc 'bias data) *bias*))
	(output       (or (assoc 'output data) #f))
	(function     (or (assoc 'function data) (lambda (inputs weights)
						   (apply + (map * inputs weights)))))
	(value #f))
    (lambda (operation)
      (case operation			; This could be a symbol or a list of values as inputs
	((input-nodes) (lambda (op . rest) (if (eq? op 'get) input-nodes
					       (if (eq? op 'set!) (set! input-nodes rest) (error "Unknown option -- input-nodes")))))
	((weights) (lambda (op . rest) (if (eq? op 'get) weights
					   (if (eq? op 'set!) (set! weights rest) (error "Unknown option -- weights")))))
	((output) (lambda (op . rest) (if (eq? op 'get) output
					  (if (eq? op 'set!) (set! output (car rest)) (error "Unknown option -- output")))))
	((bias) (lambda (op . rest) (if (eq? op 'get) bias
					(if (eq? op 'set!) (set! bias (car rest)) (error "Unknown option -- bias")))))
	((function) (lambda (op . rest) (if (eq? op 'get) function
					    (if (eq? op 'set!) (set! function (car rest)) (error "Unknown option -- function")))))
	((value) (lambda (op . rest) (if (eq? op 'get) value
					 (if (eq? op 'set!) (set! value (car rest)) (error "Unknown option -- value")))))
	((reset) (set! value #f) (map (lambda (node) (node 'reset)) input-nodes))

	((run) (begin
		 (if value value
		     (if (>= (function (map (lambda (node) (node 'run)) input-nodes) weights) bias) 
			 (begin
			   (set! value *true-signal*)
			   (if output (output *true-signal*) *true-signal*))
			 (begin
			   (set! value *false-signal*)
			   (if output (output *false-signal*) *false-signal*))))))

	(else (if (or (not (pair? operation)) (not (= (length operation) (length weights))))
		  (error "Wrong input to neural node!")
		  (if (>= (function operation weights) bias)
		      *true-signal*
		      *false-signal*)))))))



	