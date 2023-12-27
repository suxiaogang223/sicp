#lang racket/base

(require "environment.rkt")
(require "procedure.rkt")
(require "eval.rkt")

(define the-global-environment (setup-environment))

(define hello-prompt "scheme v0.1")

(define input-prompt "> ")

(define output-prompt "")

(define (prompt-for-input string)
  (newline) (display string))

(define (announce-output string)
  (display string))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (eval input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; start point
(display hello-prompt)
(driver-loop)
