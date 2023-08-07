#lang racket

(require minikanren/mk) 

(provide failo passo oro ando fliprel) 

(define failo (== 0 1))
(define passo (== 0 0))

(define (oro . ks)
  (if (null? ks)
      failo
      (conde [(car ks)]
             [(apply oro (cdr ks))])))

(define (ando . ks)
  (if (null? ks)
      passo
      (fresh ()
        (car ks)
        (apply ando (cdr ks)))))

(define (fliprel rel)
  (lambda (x y a)
    (rel y x a)))
