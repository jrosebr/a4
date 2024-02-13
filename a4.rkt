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
       (make-closure-ds x body env))
      
      ; Application should apply and of course,
      ; natural recursion
      (`(,rator ,rand)
       (apply-closure-ds (value-of rator env) (value-of rand env))))))


(define value-of-ds 
  (λ (x env)
    (value-of x env)))

(define empty-env-ds
  (λ ()
    '()))

(define extend-env-ds
  (λ (x arg env)
    (cons `(,x . ,arg) env)))

(define apply-env-ds
  (λ (env y)
    (if (assv y env)
        (cdr (assv y env))
        (error 'apply-env-ds "unbound var ~s" y))))

(define make-closure-ds
  (λ (x body env)
  
      ; We also need to extend the environment
      `(make-clos ,body ,x ,env)))

(define apply-closure-ds
  (λ (rator rand)
    (match rator
      (`(make-clos ,body ,x ,env)
       (value-of body (extend-env-ds x rand env))))))



(value-of `((lambda (x) x) 2) (empty-env-ds))



