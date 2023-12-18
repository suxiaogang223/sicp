#lang racket/base

(provide lookup-variable-value
         extend-environment
         define-variable!
         set-variable-value!
         setup-environment)

(require "procedure.rkt")

;; represent the environemnt as a list of frames

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;; represent the frame as a hash table

(define (make-frame vars vals)
  (let ([frame (make-hash)])
    (for ([var vars]
          [val vals])
      (hash-set! frame var val))
    frame))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbond variable" var)
        (let ([frame (first-frame env)])
          (if (hash-has-key? frame var)
              (hash-ref frame var)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (hash-set! (first-frame env) var val))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbond variable" var)
        (let ([frame (first-frame env)])
          (if (hash-has-key? frame var)
              (hash-set! frame var val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (setup-environment)
  (let ([initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)])
        (define-variable! 'true #t initial-env)
        (define-variable! 'false #f initial-env)
        (define-variable! 'null '() initial-env)
        initial-env))
