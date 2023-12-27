#lang racket/base

(provide primitive-procedures)

;; primitive procedures list
(define primitive-procedures
  (list (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '> >)
        (cons '< <)
        (cons '= =)
        (cons '>= >=)
        (cons '<= <=)
        (cons 'not not)
        (cons 'exit exit)))
