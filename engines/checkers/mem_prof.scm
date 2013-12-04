(define-syntax mem-prof
  (syntax-rules ()
    ((_ expr)
     (do ((i 0 (1+ i)))
	 ((= 10000 i) (print-gc-statistics))
       (if (= 0 (remainder i 10))
	   (print-gc-statistics))
       expr))))
