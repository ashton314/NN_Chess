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

(define-net test
  ;; Input nodes
  (input-node1 (shift 1))		; `shift' ala Perl
  (input-node2 (shift 1))

  ;; Hidden layers
  (node-a (input-node1 0.1) (input-node2 0.8))
  (node-b (input-node1 0.4) (input-node2 0.6))

  ;; Output nodes
  (output
   (out1 (node-a 0.3) (node-b 0.9))
   (out2 (node-a 0.1) (node-b 0.5))))


(define-syntax define-net
  (syntax-rules ()
    ((define-net net-name node-descriptions ...)
     (define (net-name . inputs)
       (define (shift)
	 (pop! inputs))
       (compile-nodes node-descriptions ...)))))


