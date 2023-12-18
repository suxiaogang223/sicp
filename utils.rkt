#lang racket/base

(provide tagged-list?)

(require "bool.rkt")

;; the procedure indentifies lists beginning with a designated symbol
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
