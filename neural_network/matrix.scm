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

(define (matrix-* matrix vect)
  ;; Assumes multiplying n-m matrix by m-1 matrix, which I'll call a vector here
;  (format #t "MATRIX-*:~%  MATRIX: '~A'~%  VECT: '~A'~%" matrix vect)
  (vector-map (lambda (node-row) ; NOTE: There's probably room for some optimization here (e.g. eliminate calls to vector->list)
		(apply + (map * (vector->list node-row) (vector->list vect))))
	      matrix))

(define (randomize lst)
  (map (lambda (n)
	 (if (= n 0)
	     (- (random 2.0) 1) n)) lst))

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
       (define net-name)
       (let ((learning-rate _learning-rate)

	     (input-values (make-vector num-inputs 0))

	     ;; These are lists of 2D vectors
	     ;; ( #(#(a1 a2 a3) #(b1 b2 b3) ...) #( ... ))  ( layer layer ... ) layer => ((weights ...) (weights ...))
	     (hidden-layers (map (lambda (layer) (list->vector (map list->vector (map randomize layer)))) '(hidden-layer-definition ...)))
	     (hidden-layers-r '())
	     (hidden-errors (map (lambda (layer) (list->vector (map (lambda (node) (list->vector (map (lambda (nul) 0) node))) layer)))
				 '(hidden-layer-definition ...)))
	     (hidden-errors-r '())
	     (hidden-values (map (lambda (layer) (list->vector (map (lambda (node) 0) layer)))
				 '(hidden-layer-definition ...)))
	     (hidden-values-r '())

	     ;; This is a 2D vector
	     (output-layer (list->vector (map list->vector (map randomize 'output-definition))))
	     (output-errors (list->vector (map (lambda (nul) 0) 'output-definition)))
	     (output-values (list->vector (map (lambda (nul) 0) 'output-definition))))

	 (set! hidden-layers-r (reverse hidden-layers))
	 (set! hidden-errors-r (reverse hidden-errors))
	 (set! hidden-values-r (reverse hidden-values))

	 (define (feed-forward input layers values)
	   (cond
	    ((null? layers) (error "Must have some layers in the network before you can run it!"))
	    ((null? input)  (error "No input given for neural network!"))
	    (else
	     (let ((result (vector-map activation (matrix-* (car layers) input))))
;	       (format #t "car values: '~A'~%" (car values))
;	       (format #t "Layer: ~A~%" result)
	       (vector-clobber! (car values) result)
	       (if (null? (cdr layers))
		   (begin
;; 		     (write-string "Nodes after feed-forward:<<__END__\n")
;; 		     (map (lambda (layer) (vector-map (lambda (node) (pretty-print node)) layer) (newline)) hidden-layers)
;; 		     (pretty-print output-layer)
;; 		     (newline)
;; 		     (write-string "__END__\n")
		     result)
		   (feed-forward result (cdr layers) (cdr values)))))))

	 (define (transpose matrix)	; working
	   (list->vector (reverse! (map (lambda (n) (vector-map (lambda (row) (vector-ref row n)) matrix))
					(range 0 (- (vector-length (vector-ref matrix 0)) 1))))))

	 (define (back-prop weights values errors previous-weights previous-errors next-values)
	   ;; backpropogation algorithm
	   ;; `weights', `values', and `errors' are lists of layers
	   (vector-clobber! (car errors) ; calculate errors for this layer
			    (vector-mapn (lambda (this-val prev-whts)
					   (* this-val (- 1 this-val) (vector-reduce + (vector-zip * prev-whts previous-errors))))
					 (car values) (transpose previous-weights)))


	   ;; NOTE: (car weights) looks something like #(#(.1 .2) #(.3 .4) ...)
	   (vector-clobber! (car weights) ; update weights for this layer
			    (vector-mapn (lambda (node delta)
					   (vector-mapn (lambda (old-weight value)
							  (+ old-weight (* learning-rate delta value)))
							node next-values))
					 (car weights) (car errors)))

	   (if (null? (cdr weights))
	       #t			; algorithm finished
	       (back-prop (cdr weights) (cdr values) (cdr errors) (car weights) (car errors)
			  (let ((nxt-vals (cdr values)))
			    (if (null? nxt-vals)
				input-values
				(car nxt-vals))))))

	 (set! net-name
	       (lambda (op . args)
		 (case op
		   ;; Primary methods
		   ((run)
		    ;; Runs the neural network: (net 'run (list input1 input2 ...))
		    (set! input-values (list->vector (car args)))
		    (set! output-values (feed-forward input-values `(,@hidden-layers ,output-layer) `(,@hidden-values ,output-values)))
		    output-values)
		   
		   ((train)
		    ;; Runs the backprop algorithm: (net 'train (list input1 input2 ...) (list target1 target2 ...))

		    (let ((inputs (list->vector (car args)))
			  (targets (list->vector (cadr args))))
		      (set! input-values inputs)

		      ; update output deltas
		      (set! output-values (feed-forward input-values `(,@hidden-layers ,output-layer) `(,@hidden-values ,output-values)))
		      (set! output-errors (vector-zip (lambda (output target) ; vector
							(* (- target output) (- 1 output) output)) output-values targets))
		      (set! output-layer (vector-zip (lambda (node delta) (vector-zip (lambda (current-weight value)
											(+ current-weight (* learning-rate delta value)))
										      node (car hidden-values-r)))
						     output-layer output-errors))


		      ; backpropogate
		      (back-prop hidden-layers hidden-values hidden-errors output-layer output-errors
				 (let ((next-values (cdr hidden-values)))
				   (if (null? next-values)
				       input-values
				       (car next-values))))))


		   ((last-errors)
		    output-errors)

		   ;; Debugging methods
		   ((x-ray)
		    ;; Full dump
		    (format #t "-------------- FULL DUMP FOR: '~A' --------------~%" 'net-name)

		    (format #t "LAYER DUMP:~%")
		    (map (lambda (layer) (vector-map (lambda (node) (pretty-print node) (write-string " ")) layer) (display "\n---\n")) hidden-layers)
		    (vector-map (lambda (node) (pretty-print node) (write-string " ")) output-layer)
		    (newline)

		    (format #t "ERROR DUMP:~%")
		    		    (map (lambda (layer) (vector-map (lambda (node) (pretty-print node) (write-string " ")) layer) (newline)) hidden-errors)
		    (pretty-print output-errors)
		    (newline)

		    (format #t "VALUE DUMP:~%")
		    (format #t "Inputs: '~A'~%" input-values)
		    (map (lambda (layer) (vector-map (lambda (node) (pretty-print node) (write-string " ")) layer) (newline)) hidden-values)
		    (format #t "Outputs: '~A'~%" output-values)
		    (newline)

		    (if (and (equal? hidden-layers-r (reverse hidden-layers))
			     (equal? hidden-errors-r (reverse hidden-errors))
			     (equal? hidden-values-r (reverse hidden-values)))
			(format #t "Reverse lists are up-to-date.~%")
			(format #t "ERROR!! Reverse lists have lost linkage to originals!~%")))

		   ((debug-hidden-layers)
		    (pretty-print hidden-layers)
		    (newline))

		   ((debug-layers)
		    (map (lambda (layer) (vector-map (lambda (node) (pretty-print node)) layer) (newline)) hidden-layers)
		    (pretty-print output-layer)
		    (newline))
		   
		   (else
		    (error (format #f "Unknown option to neural net: '~A'~%" op)))))))))))

(define (vector-reduce func vec)
  (let ((leng (vector-length vec)))
    (define (loop counter acc)
      (if (= (+ counter 1) leng)
	  acc
	  (loop (+ counter 1) (func acc (vector-ref vec (+ counter 1))))))
    (loop 0 (vector-ref vec 0))))

(define (vector-zip func vec1 vec2)
  (let ((end-val (apply min (map vector-length (list vec1 vec2)))))
    (define (zipper i acc)
      (if (> (+ i 1) end-val)
	  (list->vector (reverse! acc))
	  (zipper (+ i 1) (cons (func (vector-ref vec1 i) (vector-ref vec2 i)) acc))))
    (zipper 0 '())))

(define (vector-mapn func . vects)
  (let* ((end-val (apply min (map vector-length vects)))
	 (acc (make-vector end-val)))
    (do ((i 0 (+ i 1)))
	((>= i end-val) acc)
      (vector-set! acc i (apply func (map (lambda (vec) (vector-ref vec i)) vects))))))

(define (vector-clobber! target vals)
  (let ((leng (vector-length target)))
    (do ((i 0 (+ i 1)))
	((= i leng) target)
      (vector-set! target i (vector-ref vals i)))))

(define (range lower upper)
  (define (loop i acc)
    (if (< i lower)
	(reverse! acc)
	(loop (- i 1) (cons i acc))))
  (loop upper '()))