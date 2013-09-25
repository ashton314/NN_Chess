;;; Feed-Forward Neural Network library, using matricies
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

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

#|
Syntax description:
(define-feed-forward-net test
  1					; learning weight
  2					; two inputs
  ((.3 .1 .4) (.7 .2 .6))		; two outputs (weights from hidden layer)
  ((.1 .2) (.4 .3) (.8 .2)))		; hidden nodes
|#


(define-syntax define-feed-forward-net
  (syntax-rules ()
    ((_ net-name _learning-rate num-inputs output-definition hidden-layer-definition ...)
     (begin
       (format #t "Hidden layers: ~A~%" '(hidden-layer-definition ...))
       (define net-name)
       (let ((learning-rate _learning-rate)

	     (input-values (make-vector num-inputs 0))

	     ;; These are lists of 2D vectors
	     (hidden-layers (map (lambda (layer) (list->vector (map list->vector layer))) '(hidden-layer-definition ...)))
	     (hidden-errors (map (lambda (layer) (list->vector (map (lambda (node) (list->vector (map (lambda (nul) 0) node))) layer))) '(hidden-layer-definition ...)))
	     (hidden-values (map (lambda (layer) (list->vector (map (lambda (node) (list->vector (map (lambda (nul) 0) node))) layer))) '(hidden-layer-definition ...)))

	     ;; This is a 2D vector
	     (output-layer (list->vector (map list->vector 'output-definition)))
	     (output-values (list->vector (map (lambda (node) (list->vector (map (lambda (nul) 0) node))) 'output-definition))))

	 (set! net-name
	       (lambda (op . args)
		 (define (matrix-* matrix vect)
		   ;; Assumes multiplying n-m matrix by m-1 matrix, which I'll call a vector here
		   (vector->map (lambda (node-row) ; NOTE: There's probably room for some optimization here (e.g. eliminate calls to vector->list)
				  (apply + (map * (vector->list node-row) (vector->list vect))))
				matrix))


		 (define (feed-forward input layers values)
		   (cond
		    ((null? layers) (error "Must have some layers in the network before you can run it!"))
		    ((null? input)  (error "No input given for neural network!"))
		    (else
		     (let ((result (vector->map activation (matrix-* (car layers) input))))
		       (set-car! values result) ; will this work?
		       (if (null? (cdr layers))
			   result
			   (feed-forward result (cdr layers) (cdr values)))))))

		 (case op
		   ((run)
		    (set! input-values (list->vector (car args)))
		    (set! output-values (feed-forward input-values `(,@hidden-layers output-layer) `(,@hidden-values output-values)))
		    output-values)

		   ((debug-layers)
		    (pretty-print hidden-layers)
		    (newline))
		   
		   (else
		    (error (format #f "Unknown option to neural net: '~A'~%" op)))))))))))