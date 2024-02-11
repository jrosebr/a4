#lang racket
;Problem 1

(define value-of
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`(* ,e1 ,e2)
       (* (value-of e1 env)
          (value-of e2 env)))
      (`,n #:when (boolean? n) n)
      (`(sub1 ,n)
       (sub1 (value-of n env)))
      (`(zero? ,n)
       (zero? (value-of n env)))
      (`(if ,cond ,then ,else)
       (if (value-of cond env) (value-of then env) (value-of else env)))
      (`(let ([,var-name ,var-expression])
          ,body)
       (value-of body (extend-env-ds var-name (value-of var-expression env) env)))
      ; Lookup the symbol y in environment
      (`,y #:when (symbol? y)
           (apply-env-ds env y))
      ; Return function
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-clos-ds x body env))
      ; Application should apply and of course,
      ; natural recursion
      (`(,rator ,rand)
       (apply-clos-ds rator rand env)))))

(define value-of-ds 
  (λ (x env)
    (value-of x (empty-env-ds))))

(define empty-env-ds
  (λ ()
    (λ (y)
      (error 'value-of "unbound ~a" y))))

(define extend-env-ds
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env-ds env y))))))

(define apply-env-ds
  (λ (env y)
    (env y)))

(define make-clos-ds
  (λ (x body env)
    (λ (arg)
      ; We also need to extend the environment
      (value-of body (extend-env-ds x arg env)))))

(define apply-clos-ds
  (λ (rator rand env)
    ((value-of rator env) (value-of rand env))))