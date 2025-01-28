#!chezscheme

(import (chezscheme))

(let ()
  (define-liquid x 0)
  (assert (eqv? x 0)))

(let ()
  (define-liquid x 1)
  (set! x 2)
  (assert (eqv? x 2)))

(let ()
  (define-liquid x 3)
  (eqv?
    (begin
      (call/cc
        (lambda (k)
          (set! x 4)
          (k)))
      x)
    3))
