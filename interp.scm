(load "mk.scm")
(load "numbers.scm")

(define lookupo
  (lambda (x env t)
    (conde
      ((fresh (y v rest)
       (== `(ext-env ,y ,v ,rest) env)
       (conde
         ((== y x) (== v t))
         ((=/= y x) (lookupo x rest t)))))
      ((fresh (defs rest)
         (== `(ext-rec ,defs ,rest) env)
         (lookup-ext-reco x defs env rest t))))))

(define lookup-ext-reco
  (lambda (x defs env rest t)
    (fresh (y lam-exp others)
      (conde
        ((== '() defs) (lookupo x rest t))
        ((== `((,y ,lam-exp) . ,others) defs)
         (conde
           ((== y x) (== `(closure ,lam-exp ,env) t))
           ((=/= y x) (lookup-ext-reco x others env rest t))))))))

#|
(define lookupo
  (lambda (x env t)
    (conde
      ((fresh (y v rest)
       (== `(ext-env ,y ,v ,rest) env)
       (conde
         ((== y x) (== v t))
         ((=/= y x) (lookupo x rest t)))))
      ((fresh (defs rest)
         (== `(ext-rec ,defs ,rest) env)
         (conde
           ((lookup-ext-reco x defs env t))
           ((not-in-defso x defs) (lookupo x rest t))))))))

(define lookup-ext-reco
  (lambda (x defs env t)
    (fresh (y lam-exp others)
      (== `((,y ,lam-exp) . ,others) defs)
      (conde
        ((== y x) (== `(closure ,lam-exp ,env) t))
        ((=/= y x) (lookup-ext-reco x others env t))))))
|#

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `(ext-env ,y ,v ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((fresh (defs rest)
         (== `(ext-rec ,defs ,rest) env)
         (not-in-defso x defs)
         (not-in-envo x rest))))))

(define not-in-defso
  (lambda (x defs)
    (conde
      ((== '() defs))
      ((fresh (y lam-exp others)
         (== `((,y ,lam-exp) . ,others) defs)
         (=/= y x)
         (not-in-defso x others))))))

(define proper-listo
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d v-a v-d)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-expo a env v-a)
         (proper-listo d env v-d))))))


(define evalo
  (lambda (exp val)
    (eval-expo exp '() val)))

(define eval-expo
  (lambda (exp env val)
    (conde
      ((symbolo exp) (lookupo exp env val))
      ((fresh (v)
         (== `(quote ,v) exp)
         (== v val)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (absento 'int-val v)))
      ((fresh (x* body)
         (== `(lambda ,x* ,body) exp)
         (== `(closure (lambda ,x* ,body) ,env) val)
         (not-in-envo 'lambda env)))
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (absento 'closure a*)
         (absento 'int-val a*)
         (proper-listo a* env val)))
      ((prim-expo exp env val))

      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) exp)
         (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
         (proper-listo rands env a*)
         (ext-env*o x* a* env^ res)
         (eval-expo body res val)))

      
      ((fresh (p-name x other-xs body others letrec-body)
         (== `(letrec ((,p-name (lambda (,x . ,other-xs) ,body)) . ,others)
                ,letrec-body)
             exp)
         (not-in-envo 'letrec env)
         (eval-expo letrec-body
                    `(ext-rec ((,p-name (lambda (,x . ,other-xs) ,body)) . ,others) ,env)
                    val)))

      )))

(define ext-env*o
  (lambda (x* a* env out)
    (conde
      ((== '() x*) (== '() a*) (== env out))
      ((fresh (x a dx* da* env2)
         (== `(,x . ,dx*) x*)
         (== `(,a . ,da*) a*)
         (== `(ext-env ,x ,a ,env) env2)
         (ext-env*o dx* da* env2 out))))))

(define prim-expo
  (lambda (exp env val)
    (conde
      ((boolean-primo exp env val))
;      ((number-primo exp env val))
;      ((sub1-primo exp env val))
;      ((zero?-primo exp env val))
      ((null?-primo exp env val))
;      ((*-primo exp env val))    
      ((cons-primo exp env val))
      ((car-primo exp env val))
      ((cdr-primo exp env val))
      ((not-primo exp env val))
      ((if-primo exp env val)))))

(define boolean-primo
  (lambda (exp env val)
    (conde
      ((== #t exp) (== #t val))
      ((== #f exp) (== #f val)))))

(define cons-primo
  (lambda (exp env val)
    (fresh (a d v-a v-d)
      (== `(cons ,a ,d) exp)
      (== `(,v-a . ,v-d) val)
      (absento 'closure val)
      (absento 'int-val val)
      (not-in-envo 'cons env)
      (eval-expo a env v-a)
      (eval-expo d env v-d))))

(define car-primo
  (lambda (exp env val)
    (fresh (p d)
      (== `(car ,p) exp)
      (=/= 'int-val val)
      (=/= 'closure val)
      (not-in-envo 'car env)
      (eval-expo p env `(,val . ,d)))))

(define cdr-primo
  (lambda (exp env val)
    (fresh (p a)
      (== `(cdr ,p) exp)
      (=/= 'int-val a)
      (=/= 'closure a)
      (not-in-envo 'cdr env)
      (eval-expo p env `(,a . ,val)))))

(define not-primo
  (lambda (exp env val)
    (fresh (e b)
      (== `(not ,e) exp)
      (conde
        ((== #t b) (== #f val))
        ((== #f b) (== #t val)))         
      (not-in-envo 'not env)
      (eval-expo e env b))))

(define number-primo
  (lambda (exp env val)
    (fresh (n)
      (== `(int-exp ,n) exp)
      (== `(int-val ,n) val)
      (not-in-envo 'int-exp env))))

(define sub1-primo
  (lambda (exp env val)
    (fresh (e n n-1)
      (== `(sub1 ,e) exp)
      (== `(int-val ,n-1) val)
      (not-in-envo 'sub1 env)
      (eval-expo e env `(int-val ,n))
      (minuso n '(1) n-1))))

(define zero?-primo
  (lambda (exp env val)
    (fresh (e n)
      (== `(zero? ,e) exp)
      (conde
        ((zeroo n) (== #t val))
        ((poso n) (== #f val)))
      (not-in-envo 'zero? env)
      (eval-expo e env `(int-val ,n)))))

(define null?-primo
  (lambda (exp env val)
    (fresh (e v)
      (== `(null? ,e) exp)
      (conde
        ((== '() v) (== #t val))
        ((=/= '() v) (== #f val)))
      (not-in-envo 'null? env)
      (eval-expo e env v))))

(define *-primo
  (lambda (exp env val)
    (fresh (e1 e2 n1 n2 n3)
      (== `(* ,e1 ,e2) exp)
      (== `(int-val ,n3) val)
      (not-in-envo '* env)
      (eval-expo e1 env `(int-val ,n1))
      (eval-expo e2 env `(int-val ,n2))
      (*o n1 n2 n3))))

(define if-primo
  (lambda (exp env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) exp)
      (not-in-envo 'if env)
      (eval-expo e1 env t)
      (conde
        ((== #t t) (eval-expo e2 env val))
        ((== #f t) (eval-expo e3 env val))))))
