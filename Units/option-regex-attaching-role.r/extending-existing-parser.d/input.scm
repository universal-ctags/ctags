(define-module foo
  (use bar)
  (export baz0 baz1))
(select-module foo)
(define (foobar)
  1)
