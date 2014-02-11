;;; Feed-Forward Neural Network library, using matricies
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)
(declare (usual-integrations)
	 (integrate-external "vector_utilities"))

(define *debug* #f)

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

(define-integrable (sigmoid val)
  ;; Sigmoid function
  (/ 1 (+ 1 (exp (- val)))))

(define-integrable (sigmoid-derivitive val)
  ;; d/dx sigmoid
  (* val (- 1 val)))

#|
Usage:
(define-ffn .7 2
  '(3 2))

(define-ffn .7 2
  '(((.2 .3 .1) (.5 .4 .7) (.2 .3 .1))
    2))
|#

(declare (integrate-operator define-ffn))
(define define-ffn
  (lambda (learning-rate input-count layers-definition . options)
    ;; returns quasi-object closure
    (let ((layers (initialize-layers input-count layers-definition)))
      (lambda (op . args)
	(case op
	  ;; primary routines
	  ((run) (car (forward-prop (car args) layers)))
	  ((run-with-values) (forward-prop (car args) layers))

	  ((genetic-code) (error "option `genetic-code' not implemented")) ; returns genetic code to network (for genetic algorithms)

	  ;; setters
	  ((set-learning-rate) (set! learning-rate (car args)))
	  ((set-layers) (set! layers (car args)))

	  ;; getters
	  ((get-learning-rate) learning-rate)
	  ((get-layers) layers)

	  ;; debugging routines
	  ((pp-layers) (map (lambda (layer) (write-string "----------\n") (pp layer)) layers)))))))


(define-integrable (forward-prop inputs all-layers)
  (define (loop input layers value-acc)	; values-acc holds node values in REVERSE ORDER
    (if (null? layers)			; forward pass finished
	(cons input value-acc)
	(let ((values (map (lambda (node) ; add bias here -----V
			     (sigmoid (reduce + 0 (map * (cons 1 inputs) node))))
			   (car layers))))
	  (loop values (cdr layers) (cons values values-acc)))))
  (loop inputs all-layers '()))



;; utility routines
(define-integrable (initialize-layers input-count all-layers)
  (define (loop previous-input-count layers acc)
    (if (null? layers)
	(reverse! acc)
	(cond ((integer? (car layers))
	       (loop (car layers) (cdr layers)
		     (cons (map		                                          ; cons->vector mods here
			    (lambda (node)
			      (randomize (make-list (+ previous-input-count 1) 0))) ; cons->vector mods here
			    (make-list (car layers)))
			   acc)))

	      ((and (pair? (car layers))
		    (for-all? (car layers) list?)) ; weights specified
	       (loop (length (car layers)) (cdr layers)
		     (cons (map
			    (lambda (node)
			      (if (not (= (length node) (+ 1 previous-inputs)))
				  (error (format #f "too few weights specified in layer ~A (you might have forgotten to account for the bias)" node))
				  (if (for-all? node (lambda (n) (and (integer? n) (= n 0))))
				      (randomize node)                            ; cons->vector mods here
				      node)))                                     ; cons->vector mods here
			    (car layers))
			   acc)))

	      (else (error "Malformed argument list to initialize-layers (probably from a call to define-ffn)")))))
  (loop input-count all-layers '()))
