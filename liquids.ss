#!r6rs

(import
  (rename (rnrs)
    (call/cc rnrs:call/cc)
    (call-with-current-continuation rnrs:call-with-current-continuation))
  (only (scheme)
    make-thread-parameter
    call-with-immediate-continuation-mark
    with-continuation-mark))

(define current-liquids
  (make-thread-parameter '()))
(define current-dirty-liquids
  (make-thread-parameter '()))
(define cache-clean?
  (lambda ()
    (eq? (current-dirty-liquids) '())))

(define sentinel (list 'sentinel))

(define make-liquid
  (lambda (init)
    (let ([cell (make-thread-parameter sentinel)])
      (define liquid
        (case-lambda
          [()
           (let ([val (cell)])
             (cond
               [(not (eq? val sentinel)) val]
               [else
                 (let ([val
                         (cond
                           [(assq cell (current-liquids)) => cdr]
                           [else init])])
                   (cell val)
                   (current-dirty-liquids (cons cell (current-dirty-liquids)))
                   val)]))]
          [(val)
           (cond
             [(not (eq? (cell) sentinel))
              (cell val)]
             [else
               (cell val)
               (current-dirty-liquids (cons cell (current-dirty-liquids)))])]))
      liquid)))

(define reset-cache!
  (lambda ()
    (for-each
      (lambda (cell)
        (cell sentinel))
      (current-dirty-liquids))
    (current-dirty-liquids '())))

(define save-cache!
  (lambda ()
    (for-each
      (lambda (cell)
        (set-liquid! cell (cell))
        (cell sentinel))
      (current-dirty-liquids))
    (current-dirty-liquids '())))

(define set-liquid!
  (lambda (liquid val)
    (let ([liquids (current-liquids)])
      (current-liquids
        (let f ([tail liquids] [rev-head '()])
          (cond
            [(null? tail)
             (cons (cons liquid val) liquids)]
            [(eq? liquid (caar tail))
             (cons (cons liquid val) (append-reverse rev-head (cdr tail)))]
            [else (f (cdr tail) (cons (car tail) rev-head))]))))))


(define append-reverse
  (lambda (rev-head tail)
    (fold-left
      (lambda (tail head)
        (cons head tail))
      tail rev-head)))

(define make-continuation
  (lambda (rnrs:k)
    (save-cache!)
    (let ([liquids (current-liquids)])
      (lambda args
        (reset-cache!)
        (current-liquids liquids)
        (apply rnrs:k args)))))

(define call-with-current-continuation
  (let ([key (list 'continuation)])
    (lambda (proc)
      (call-with-immediate-continuation-mark key
        (lambda (k)
          (if (and k (cache-clean?))
              (proc k)
              (rnrs:call-with-current-continuation
                (lambda (rnrs:k)
                  (let ([k (make-continuation rnrs:k)])
                    (with-continuation-mark key k
                      (proc k)))))))))))

;;; TODO: We need to rewrite guard, which uses call/cc under the hood.
;;; MAYBE: Remove dynamic-wind; implement delimited continuations.

;;; Tests

(define x (make-liquid 'x))
(assert (eqv? (x) 'x))

(x 1)
(assert (eqv? (x) 1))

(define y (make-liquid 'y))
(assert (eqv? (y) 'y))

(y 2)
(assert (eqv? (y) 2))
(assert (eqv? (x) 1))

(y 3)
(assert (eqv? (y) 3))
(assert (eqv? (x) 1))

(x 4)
(assert (eqv? (y) 3))
(assert (eqv? (x) 4))

(call-with-current-continuation
  (lambda (k)
    (assert (eqv? (x) 4))
    (x 5)
    (assert (eqv? (x) 5))
    (k)))
(assert (eqv? (x) 4))

(let ([c
        (call-with-current-continuation
          (lambda (k)
            (assert (eqv? (x) 4))
            (x 5)
            ((call-with-current-continuation
               (lambda (c)
                 (assert (eqv? (x) 5))
                 (lambda ()
                   (k c)))))))])
  (assert (eqv? (x) 4))
  (x 6)
  (call-with-current-continuation
    (lambda (abort)
      (lambda () (abort (assert (eqv? (x) 5))))))
  (assert (eqv? (x) 6)))

(call-with-current-continuation
  (lambda (k1)
    (call-with-current-continuation
      (lambda (k2)
        (assert (eq? k1 k2))))))

(call-with-current-continuation
  (lambda (k1)
    (x 7)
    (call-with-current-continuation
      (lambda (k2)
        (assert (not (eq? k1 k2)))))))
(assert (eqv? (x) 7))
