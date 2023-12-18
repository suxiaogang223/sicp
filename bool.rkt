#lang racket/base

(provide true
         false
         true?
         false?)

(define true #t)

(define false #f)

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))
