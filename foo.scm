;; file to test my makefile with

(declare (usual-integrations))

(define-integrable (foo n)
  (if (= n 0) 0 (* n (foo (- n 1)))))
