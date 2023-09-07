(define (self-evaluation ? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
n
(define (variable? exp) (symbol? exp))

;; the procedure indentifies lists beginning with a designated symbol
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; > (quote? (quote a))
;; #f
;; > (quote? '(quote a))
;; #t
;; > (quote? 'a)
;; #f
;; > (quote? ''a)
;; #t
(define (quote? exp)
  (tagged-list? exp 'quote))

;; exp must be quote, otherwise, unexpected results may occur
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

;; (cadr exp) => (car (cdr exp))
(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; defion
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (caddr exp)))
      (caddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr exp)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        ((else (make-begin seq)))))

(define (make-begin seq) (cons 'begin seq))

;; application
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; cond form looks like this:
;; (cond ((> x 0) x)
;;       ((= x 0) (display ’zero) 0)
;;       (else (- x)))
;;
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

;; cond will be tranformed to if
;; (if (> x 0)
;;     x
;;     (if (= x 0)
;;         (begin (display 'zero)
;;                0)
;;         (- x)))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expan-clauses clauses)
  (if (null? clauses)
      'false
      (let )))
