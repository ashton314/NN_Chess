;;; Common macros
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(declare (usual-integrations)
	 (integrate-operator call-once while dotimes inc! push! pop! this-and-next))

(define-syntax call-once
  ;; Takes a thunk, and calls the thunk once. Next iteration, (or whatever) the thunk is NOT called
  (syntax-rules ()
    ((_ thunk)
     (error "not implemented"))))

(define-syntax while
  (syntax-rules ()
    ((_ condition body ...)
     (do ()
	 ((not condition) #f)
       body ...))))

(define-syntax dotimes
  (syntax-rules ()
    ((_ (var times) body ...)
     (do ((var 0 (+ var 1)))
	 ((= var times) #t)
       body ...))))

(define-syntax inc!
  (syntax-rules ()
    ((_ var val) (begin (set! var (+ var val)) var))
    ((_ var) (begin (set! var (+ var 1)) var))))

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

(define-syntax this-and-next
  ;; Iterates over a list, binding the current value and the next to
  ;; the two supplied arguments
  (syntax-rules ()
    ((_ (var1 var2 src-list) body ...)
     (let ((null #f))
       (define (loop var1 var2 the-rest)
	 (begin body ...)
	 (if (null? (cdr the-rest))
	     #f
	     (loop (car the-rest) (cadr the-rest) (cdr the-rest))))
       (let ((source src-list))
	 (loop (car source) (cadr source) (cdr source)))))))
