#lang racket

(require minikanren/mk minikanren/matche)

;; (define-syntax matches
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       [(matches (v) clause ...) (matche (v) clause ...)]
;;       [()])))
