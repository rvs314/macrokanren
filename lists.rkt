#lang racket

(require minikanren/mk minikanren/matche minikanren/numbers "base.rkt")

(provide nullo caro cdro conso)

(define (nullo x)
  (== x '()))

(define (caro x y)
  (fresh (q)
    (== x (cons y q))))

(define (cdro x y)
  (fresh (q)
    (== x (cons q y))))

(define (conso x y z)
  (== (cons x y) z))

(provide append-reverseo reverseo appendo concato)

(define (append-reverseo xs ys res)
  (matche xs
    [()         (== ys res)]
    [(,a . ,as) (append-reverseo as (cons a ys) res)]))

(define (reverseo xs ys)
  (append-reverseo xs '() ys))

(define (appendo l1 l2 rst)
  (fresh (r1)
    (reverseo l1 r1)
    (append-reverseo r1 l2 rst)))

(define (concato lists res)
  (matche lists
    [()         (== res '())]
    [(,a . ,as) (fresh (q)
                  (appendo a q res)
                  (concato as q))]))

(provide membero selecto lengtho)

(define (membero needle haystack)
  (matche haystack
    [(,h . ,hs)
     (conde [(== h needle)]
            [(membero needle hs)])]))

(define (inserto x lst lst+x)
  (conde
    [(== lst+x (cons x lst))]
    [(fresh (q a d)
       (== lst (cons a d))
       (inserto x d q)
       (== lst+x (cons a q)))]))

(define (selecto x lst lst+x)
  (matche lst+x
    [(,k . ,ks) (== x k) (== ks lst)]
    [(,k . ,ks) (fresh (ys)
                  (selecto x ys ks)
                  (== lst (cons k ys)))]))

(define (lengtho lst k)
  (matche lst
    [() (zeroo k)]
    [(,?? . ,as)
     (fresh (n)
       (lengtho as n)
       (pluso '(1) n k))]))

(provide andmapo ormapo)

(define-syntax andmapo
  (lambda (k)
    (syntax-case k ()
      [(_ fn xs ...)
       (with-syntax ([(crs ...) (generate-temporaries #'(xs ...))]
                     [(cds ...) (generate-temporaries #'(xs ...))]
                     [(ks ...)  (generate-temporaries #'(xs ...))])
         #'(let loop ([ks xs] ...)
             (fresh (crs ... cds ...)
               (conde [(== '() ks) ...]
                      [(== ks (cons crs cds))
                       ...
                       (fn crs ...)
                       (loop cds ...)]))))])))

(define-syntax ormapo
  (lambda (k)
    (syntax-case k ()
      [(_ fn xs ...)
       (with-syntax ([(crs ...) (generate-temporaries #'(xs ...))]
                     [(cds ...) (generate-temporaries #'(xs ...))]
                     [(ks ...)  (generate-temporaries #'(xs ...))])
         #'(let loop ([ks xs] ...)
             (fresh (crs ... cds ...)
               (== ks (cons crs cds))
               ...
               (conde [(fn crs ...)]
                      [(loop cds ...)]))))])))

(provide same-lengtho)

(define (same-lengtho xs ys)
  (matche (xs ys)
    [(() ())]
    [((,?? . ,a) (,?? . ,b))
     (same-lengtho a b)]))

(provide fold-lefto fold-righto)

(define (fold-lefto fn a xs res)
  (matche xs
    [() (== a res)]
    [(,k . ,ks)
     (fresh (q)
       (fn k a q)
       (fold-lefto fn q ks res))]))

(define (fold-righto fn a xs res)
  (matche xs
    [() (== a res)]
    [(,k . ,ks)
     (fresh (q)
       (fold-righto fn a ks q)
       (fn k q res))]))

(provide list-refo transposeo)

(define (list-refo lst n res)
  (matche lst
    [(,a . ,??) (zeroo n) (== res a)]
    [(,?? . ,b) (fresh (k)
                  (pluso k (build-num 1) n)
                  (list-refo b k res))]))

(define (transposeo xs ts)
  (conde
    [(andmapo nullo xs) (== ts '())]
    [(fresh (cars cdrs tks)
       (andmapo caro xs cars)
       (andmapo cdro xs cdrs)
       (transposeo cdrs tks)
       (== ts (cons cars tks)))]))

(provide all-sameo remove-allo remove-duplicateso)

(define (all-sameo xs)
  (define (loopo x xs)
    (matche xs
      [()]
      [(,a . ,d) (== a x) (loopo x d)]))
  (matche xs
    [()]
    [(,??)]
    [(,a . ,ds) (loopo a ds)]))

(define (remove-allo k xs res)
  (matche xs
    [()        (== res '())]
    [(,a . ,d) (fresh (q)
                 (remove-allo k d q)
                 (conde
                   [(==  k a) (== res q)]
                   [(=/= k a) (== res (cons a q))]))]))

(define (remove-duplicateso xs ys)
  (matche xs
    [() (== ys '())]
    [(,x . ,as)
     (fresh (ks kks)
       (remove-allo x as ks)
       (remove-duplicateso ks kks)
       (== ys (cons x kks)))]))
