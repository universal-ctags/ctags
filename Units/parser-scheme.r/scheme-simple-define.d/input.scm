(define a0 1)
(define a1 "a")
(define a2 '(a))
(define a3 '(a b))
(define a4 '(a . b))
(define a5 '(a b c))
(define a6 '(a b . c))
(define a7 '(a
	     b . c))
(define a8
  '(a b . c))

(define
  a9 '(a b . c))

(define
  aa
  '(a b . c))

(define (b0) 1)
(define (b1 a) 1)
(define (b2 a b) 1)
(define (b3 a . b) 1)
(define (b4 a
	    b) 1)
(define (b5
	 a b) 1)
(define (b6
	 a . b) 1)
(define (b7
	 a . b)
  1)

(define (
	 b8
	 a . b)
  1)

(define
  (
   b9
   a . b)
  1)

(define
  (ba
   a . b)
  1)

(define ((c0)) 1)
(define ((c1) a) 1)
(define ((c2) a b) 1)
(define ((c3) a . b) 1)
(define ((c4) a
	 b) 1)
(define ((c5)
	 a b) 1)
(define ((c6)
	 a . b) 1)
(define ((c7)
	 a . b)
  1)

(define ((
	  c8)
	 a . b)
  1)

(define (
	 (
	  c9)
	 a . b)
  1)

(define
  (
   (
    ca)
   a . b)
  1)

;; incomplete input
(define 
