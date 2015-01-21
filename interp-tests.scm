(load "interp.scm")
(load "test-check.scm")

;; TODO
;;
;; add factorial test
;;
;; add unit tests for cons, cdr, letrec, cond, etc


(test "pluso-1"
  (run 5 (x y z)
    (pluso x y z))
  '((_.0 () _.0)
    (() (_.0 . _.1) (_.0 . _.1))
    ((1) (1) (0 1))
    ((1) (0 _.0 . _.1) (1 _.0 . _.1))
    ((1) (1 1) (0 0 1))))

(test "*o-1"
  (run* (q)
    (fresh (x y)
      (*o x y (build-num 24))
      (== `(,x ,y ,(build-num 24)) q)))
  '(((1) (0 0 0 1 1) (0 0 0 1 1))
    ((0 0 0 1 1) (1) (0 0 0 1 1))
    ((0 1) (0 0 1 1) (0 0 0 1 1))
    ((0 0 1) (0 1 1) (0 0 0 1 1))
    ((0 0 0 1) (1 1) (0 0 0 1 1))
    ((1 1) (0 0 0 1) (0 0 0 1 1))
    ((0 1 1) (0 0 1) (0 0 0 1 1))
    ((0 0 1 1) (0 1) (0 0 0 1 1))))


(test "quines-1"
  (run 5 (q) (evalo q q))
  '(#t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0))) '(lambda (_.0) (list _.0 (list 'quote _.0)))) (=/= ((_.0 closure)) ((_.0 int-val)) ((_.0 list)) ((_.0 quote))) (sym _.0))
    (((lambda (_.0) (list (car _.0) (list 'quote _.0))) '((lambda (_.0) (list (car _.0) (list 'quote _.0))) . _.1)) (=/= ((_.0 car)) ((_.0 closure)) ((_.0 int-val)) ((_.0 list)) ((_.0 quote))) (sym _.0) (absento (closure _.1) (int-val _.1)))
    (((lambda (_.0) (list _.0 (list (car '(quote . _.1)) _.0))) '(lambda (_.0) (list _.0 (list (car '(quote . _.1)) _.0)))) (=/= ((_.0 car)) ((_.0 closure)) ((_.0 int-val)) ((_.0 list)) ((_.0 quote))) (sym _.0) (absento (closure _.1) (int-val _.1)))))


(test "I love you 20"
  (run 20 (q)
    (evalo
     q
     '(I love you)))
  '('(I love you)
    (list 'I 'love 'you)
    ((car '((I love you) . _.0))
     (absento (closure _.0) (int-val _.0)))
    (cons 'I '(love you))
    ((lambda () '(I love you)))
    ((letrec ((_.0 (lambda (_.1 . _.2) _.3))) '(I love you))
     (=/= ((_.0 quote))))
    ((cdr '(_.0 I love you))
     (absento (closure _.0) (int-val _.0)))
    ((letrec ((_.0 (lambda (_.1 . _.2) _.3))
              (_.4 _.5))
       '(I love you))
     (=/= ((_.0 quote)) ((_.4 quote))))
    ((list (car '(I . _.0)) 'love 'you)
     (absento (closure _.0) (int-val _.0)))
    ((list 'I 'love (car '(you . _.0)))
     (absento (closure _.0) (int-val _.0)))
    ((letrec ((_.0 (lambda (_.1 . _.2) _.3))
              (_.4 _.5)
              (_.6 _.7))
       '(I love you))
     (=/= ((_.0 quote)) ((_.4 quote)) ((_.6 quote))))
    (car (list '(I love you)))
    ((list 'I (car '(love . _.0)) 'you)
     (absento (closure _.0) (int-val _.0)))
    (((lambda (_.0) _.0) '(I love you)) (sym _.0))
    ((letrec ((_.0 (lambda (_.1 . _.2) _.3))
              (_.4 _.5)
              (_.6 _.7)
              (_.8 _.9))
       '(I love you))
     (=/= ((_.0 quote)) ((_.4 quote)) ((_.6 quote))
          ((_.8 quote))))
    ((letrec ((_.0 (lambda (_.1 . _.2) _.3))
              (_.4 _.5)
              (_.6 _.7)
              (_.8 _.9)
              (_.10 _.11))
       '(I love you))
     (=/= ((_.0 quote)) ((_.10 quote)) ((_.4 quote))
          ((_.6 quote)) ((_.8 quote))))
    ((list 'I 'love (cdr '(_.0 . you)))
     (absento (closure _.0) (int-val _.0)))
    (((lambda (_.0) '(I love you)) '_.1) (=/= ((_.0 quote)))
     (absento (closure _.1) (int-val _.1)))
    ((list (cdr '(_.0 . I)) 'love 'you)
     (absento (closure _.0) (int-val _.0)))
    (list 'I 'love ((lambda () 'you)))))

(test "I love you 99"
  (length (run 99 (q)
            (evalo
             q
             '(I love you))))
  99)


(test "append-1"
  (run* (q)
    (evalo
     '(letrec ((append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '((a b c d e)))

(test "append-2"
  (run 3 (q)
    (evalo
     `(letrec ((append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append '(a b c) ,q))
     '(a b c d e)))
  '('(d e)
    (list 'd 'e)
    ((letrec ((_.0 (lambda (_.1 . _.2) _.3))) '(d e))
     (=/= ((_.0 quote))))))

(test "append-3"
  (run* (q)
    (evalo
     `(letrec ((append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append '(a b c) (quote ,q)))
     '(a b c d e)))
  '((d e)))

(test "append-4"
  (run* (q)
    (evalo
     `(letrec ((append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append (quote ,q) '(d e)))
     '(a b c d e)))
  '((a b c)))

(test "append-5"
  (run* (l s)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,l) (quote ,s)))
     '(a b c d e)))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))

(test "append-6"
  (run 6 (x y z)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,x) (quote ,y)))
     `(a b c . ,z)))
  '(((() (a b c . _.0) _.0)
     (absento (closure _.0) (int-val _.0)))
    (((a) (b c . _.0) _.0)
     (absento (closure _.0) (int-val _.0)))
    (((a b) (c . _.0) _.0)
     (absento (closure _.0) (int-val _.0)))
    (((a b c) _.0 _.0)
     (absento (closure _.0) (int-val _.0)))
    (((a b c _.0) _.1 (_.0 . _.1))
     (absento (closure _.0) (closure _.1)
              (int-val _.0) (int-val _.1)))
    (((a b c _.0 _.1) _.2 (_.0 _.1 . _.2))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (int-val _.0) (int-val _.1) (int-val _.2)))))

(test "append-7"
  (run 6 (x y z)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote (a . ,x)) (quote ,y)))
     `(a b c . ,z)))
  '(((() (b c . _.0) _.0)
     (absento (closure _.0) (int-val _.0)))
    (((b) (c . _.0) _.0)
     (absento (closure _.0) (int-val _.0)))
    (((b c) _.0 _.0)
     (absento (closure _.0) (int-val _.0)))
    (((b c _.0) _.1 (_.0 . _.1))
     (absento (closure _.0) (closure _.1) (int-val _.0)
              (int-val _.1)))
    (((b c _.0 _.1) _.2 (_.0 _.1 . _.2))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (int-val _.0) (int-val _.1) (int-val _.2)))
    (((b c _.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (int-val _.0) (int-val _.1)
              (int-val _.2) (int-val _.3)))))

(test "append-8"
  (run* (x y z)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote (e . ,x)) (quote ,y)))
     `(a b c . ,z)))
  '())

(test "append-9"
  (run 4 (x y z)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,x) (quote (c . ,y))))
     `(a b c . ,z)))
  '((((a b) _.0 _.0)
     (absento (closure _.0) (int-val _.0)))
    (((a b c) _.0 (c . _.0))
     (absento (closure _.0) (int-val _.0)))
    (((a b c _.0) _.1 (_.0 c . _.1))
     (absento (closure _.0) (closure _.1)
              (int-val _.0) (int-val _.1)))
    (((a b c _.0 _.1) _.2 (_.0 _.1 c . _.2))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (int-val _.0) (int-val _.1) (int-val _.2)))))



(test "append-underspecified-1"
  ;; hmmm -- need to be super careful when using eigen.  Are these
  ;; eigen uses safe?  Are they being used with =/= or absento?
  (run 1 (append-body)
    (fresh (?)
      (eigen (a b c d e)
        (== `(lambda (l s)
               (if (null? l)
                   s
                   (cons (car l) (append (cdr l) s))))
            append-body)
        (evalo
         `(letrec ((append ,append-body))
            (append (quote (,a ,b ,c)) (quote (,d ,e))))
         (list a b c d e)))))
  '((lambda (l s)
      (if (null? l) s (cons (car l) (append (cdr l) s))))))

(test "append-underspecified-2"
  (run 1 (append-body)
  ;; hmmm -- need to be super careful when using eigen.  Are these
  ;; eigen uses safe?  Are they being used with =/= or absento?    
    (fresh (?)
      (eigen (a b c d e)
        (== `(lambda (l s)
               (if (null? l)
                   s
                   (cons (car l) (append ,? s))))
            append-body)
        (evalo
         `(letrec ((append ,append-body))
            (append (quote (,a ,b ,c)) (quote (,d ,e))))
         (list a b c d e)))))
  '((lambda (l s)
      (if (null? l) s (cons (car l) (append (cdr l) s))))))


(printf "The test 'append-underspecified-3' is expected to fail!\n")

;; this failing test shows why we should ideally find the smallest
;; program that satisfies the example, rather than letting miniKanren
;; do the least amount of work to find a solution.
(test "append-underspecified-3"
  (run 1 (append-body)    
    (fresh (?)
      ;; hmmm -- need to be super careful when using eigen.  Are these
      ;; eigen uses safe?  Are they being used with =/= or absento?  
      (eigen (a b c d e)
        (== `(lambda (l s)
               (if (null? l)
                   s
                   (cons (car l) ,?)))
            append-body)
        (evalo
         `(letrec ((append ,append-body))
            (append (quote (,a ,b)) (quote (,c))))
         (list a b c)))))
  '((lambda (l s)
      (if (null? l)
          s
          (cons (car l) (append (cdr l) s))))))
;; miniKanren generates this overly-specific answer:
;;
;; ((lambda (l s)
;;    (if (null? l)
;;        s
;;        (cons (car l) (list (car (cdr l)) (car s))))))



;; generate car/cdr sequences to find the element in a tree

(test "generate tree accessor sequence 1"
  (run 1 (q)
    (absento 8 q)
    (evalo `((lambda (x) ,q) (quote (8))) 8))
  '((car x)))

(test "generate tree accessor sequence 1 b"
  (run 10 (q)
    (absento 8 q)
    (evalo `((lambda (x) ,q) (quote (8))) 8))
  '((car x)
    ((lambda () (car x)))
    (car (list (car x)))
    (car (list (car x) x))
    ((car (letrec ((_.0 (lambda (_.1 . _.2) _.3))) x))
     (=/= ((_.0 x)))
     (absento (8 _.0) (8 _.1) (8 _.2) (8 _.3)))
    ((car (list (car x) '_.0))
     (absento (8 _.0) (closure _.0) (int-val _.0)))
    (car (list (car x) x x))
    (car ((lambda () x)))
    ((car (list (car x) (lambda _.0 _.1)))
     (absento (8 _.0) (8 _.1) (closure _.0) (closure _.1)
              (int-val _.0) (int-val _.1)))
    ((car (letrec ((_.0 (lambda (_.1 . _.2) _.3)) (_.4 _.5)) x))
     (=/= ((_.0 x)) ((_.4 x)))
     (absento (8 _.0) (8 _.1) (8 _.2) (8 _.3) (8 _.4) (8 _.5)))))

(test "generate tree accessor sequence 2"
  (run 1 (q) 
    (absento 8 q)
    (evalo `((lambda (x) ,q) (quote (7 8))) 8))
  '((car (cdr x))))

(test "generate tree accessor sequence 3 broken"
  ;; without the absento to keep miniKanren honest, miniKanren will cheat!
  (run 1 (q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((quote 7)))

(test "generate tree accessor sequence 3 working"
  ;; the absento ensures mk will be honest
  ;;
  ;; however, we would ideally like q only to contain calls to car and
  ;; cdr, and probably not include letrec etc.  How to best specify
  ;; this, which could also greatly limit the branching factor and
  ;; speed up the search?  Can we specify which parts of the language
  ;; are fair game for any given subexpression somehow?  Maybe through
  ;; environment games?
  (run 1 (q)
    (absento 7 q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

;; Here's one way to specify that we only want car & cdr
(define accessoro
  (lambda (t x)
    (conde
      ((== `(car ,x) t))
      ((== `(cdr ,x) t))
      ((fresh (c?r t1)
         (== `(,c?r ,t1) t)
         (conde
           ((== 'car c?r))
           ((== 'cdr c?r)))
         (accessoro t1 x))))))

(test "accessoro-1"
  (run 10 (q x)
    (accessoro q x))
  '(((car _.0) _.0)
    ((cdr _.0) _.0)
    ((car (car _.0)) _.0)
    ((cdr (car _.0)) _.0)
    ((car (cdr _.0)) _.0)
    ((cdr (cdr _.0)) _.0)
    ((car (car (car _.0))) _.0)
    ((cdr (car (car _.0))) _.0)
    ((car (cdr (car _.0))) _.0)
    ((cdr (cdr (car _.0))) _.0)))


;; While the following two tests are much faster than 'generate tree
;; accessor sequence 3 working', they require a helper function *and*
;; the queries are still not refutationally complete (since they are
;; generate & test).  Is there a better way to restrict the desired
;; language for any given subexpression?  That would be extremely
;; useful.
;;
;; Maybe the fixpoint trick Oleg used for his relational type
;; inferencer would work.
;;
;; In any case, would like refutational completeness, and want to get
;; real pruning for performance, rather than generate & test.
(test "generate tree accessor sequence 3 working fast"
  (run 1 (q)
    (accessoro q 'x)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

(test "generate tree accessor sequence 3 working fast no lambda"
  (run 1 (q)
    (accessoro q '(quote (8 (7) 9)))
    (evalo q 7))
  '((car (car (cdr '(8 (7) 9))))))

(test "generate tree accessor sequence 4 working fast"
  (run 1 (q)
    (accessoro q 'x)
    (evalo `((lambda (x) ,q) (quote (8 ((6 (((((7))))))) 9))) 7))
  '((car (car (car (car (car (car (cdr (car (car (cdr x))))))))))))


(test "generate tree accessor sequence 3 working, enforce no lambda in 'q'"
  ;; the (absento 'lambda q) speeds up the search significantly
  (run 1 (q)
    (absento 7 q)
    (absento 'lambda q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

(test "generate tree accessor sequence 3 working, 3 absentos"
  (run 1 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

(test "generate tree accessor sequence 3 working, 4 absentos"
  (run 1 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (absento 'if q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

(test "generate tree accessor sequence 3 working, 5 absentos"
  (run 1 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (absento 'if q)
    (absento 'null? q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

(test "generate tree accessor sequence 3 working, 6 absentos"
  ;; seems like we're starting to get a little slower at this point
  (run 1 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (absento 'if q)
    (absento 'null? q)
    (absento 'letrec q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

(test "generate tree accessor sequence 3 working, 6 absentos  b"
  ;; run 3 gives us a funny answer!
  (run 3 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (absento 'if q)
    (absento 'null? q)
    (absento 'letrec q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))
    (car (list (car (car (cdr x)))))
    (car (list (car (car (cdr x))) x))))

(test "generate tree accessor sequence 3 working, 9 absentos"
  ;; fastest yet, but still not refutationally complete
  (run 1 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (absento 'if q)
    (absento 'null? q)
    (absento 'letrec q)
    (absento 'list q)
    (absento 'quote q)
    (absento 'not q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9))) 7))
  '((car (car (cdr x)))))

#|
(test "generate tree accessor sequence 4 working, 9 absentos"
  ;; still too sloooow.  I guess there isn't much pruning, if any?
  (run 1 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (absento 'if q)
    (absento 'null? q)
    (absento 'letrec q)
    (absento 'list q)
    (absento 'quote q)
    (absento 'not q)
    (evalo `((lambda (x) ,q) (quote (8 ((6 (((((7))))))) 9))) 7))
  '((car (car (car (car (car (car (cdr (car (car (cdr x))))))))))))
|#

(test "generate tree accessor sequence 5 working, 9 absentos"
  ;; we can access two distinct 7's in this example
  (run 2 (q)
    (absento 7 q)
    (absento 'lambda q)
    (absento 'cons q)
    (absento 'if q)
    (absento 'null? q)
    (absento 'letrec q)
    (absento 'list q)
    (absento 'quote q)
    (absento 'not q)
    (evalo `((lambda (x) ,q) (quote (8 (7) 9 7))) 7))
  '((car (car (cdr x)))
    (car (cdr (cdr (cdr x))))))

(test "generate tree from accessor 1"
  ;; 
  (run* (q)
    (evalo `((lambda (x) (car (car (cdr x)))) (quote ,q)) 7))
  '(((_.0 (7 . _.1) . _.2)
     (absento (closure _.0) (closure _.1) (closure _.2)
              (int-val _.0) (int-val _.1) (int-val _.2)))))

#|
;; could potentially run an interpreter inside the interpreter to generate the car/cdr pattern.
;; maybe something like:
(letrec ((f (lambda (x ls)
              (if (null? ls)
                  x
                  (if (equal? (car ls) (quote car))
                      (f (car x) (cdr ls))
                      (if (equal? (car ls) (quote cdr))
                          (f (cdr x) (cdr ls))
                          (quote error)))))))
  (f (quote (4 5 6)) (quote (cdr car))))
|#

;; (test "1"
;;   (run* (q) (eval-expo '#f '() q))
;;   '(#f))

;; (test "2"
;;   (run* (q) (eval-expo '#t '() q))
;;   '(#t))

;; (test "3a"
;;   (run* (q) (eval-expo '(quote a) '() q))
;;   '(a))

;; (test "3b"
;;   (run* (q) (eval-expo '(quote 5) '() q))
;;   '(5))

;; (test "3c"
;;   (run* (q) (eval-expo '(quote (3 . 4)) '() q))
;;   '((3 . 4)))

;; (test "4"
;;   (run* (q) (eval-expo '(equal? (quote foo) (quote bar)) '() q))
;;   '(#f))

;; (test "5"
;;   (run* (q) (eval-expo '(equal? (quote foo) (quote foo)) '() q))
;;   '(#t))

;; (test "6a"
;;   (run* (q) (eval-expo '(symbol? (quote foo)) '() q))
;;   '(#t))

;; (test "6b"
;;   (run* (q) (eval-expo '(number? (quote foo)) '() q))
;;   '(#f))

;; (test "6c"
;;   (run* (q) (eval-expo '(number? 5) '() q))
;;   '(#t))

;; (test "7"
;;   (run* (q) (eval-expo '((lambda (x) (symbol? x)) (quote foo)) '() q))
;;   '(#t))

;; (test "8a"
;;   (run* (q) (eval-expo '(symbol? #f) '() q))
;;   '(#f))

;; (test "8b"
;;   (run* (q) (eval-expo '(symbol? 5) '() q))
;;   '(#f))

;; (test "8c"
;;   (run* (q) (eval-expo '(symbol? (quote (3 . 4))) '() q))
;;   '(#f))

;; (test "9"
;;   (run* (q) (eval-expo '((lambda (x) (symbol? x)) #t) '() q))
;;   '(#f))

;; (test "10"
;;   (run* (q) (eval-expo '(if (symbol? (quote foo)) (quote true) (quote false)) '() q))
;;   '(true))

;; (test "11"
;;   (run* (q) (eval-expo '(if (symbol? (quote #f)) (quote true) (quote false)) '() q))
;;   '(false))

;; (test "12"
;;   (run* (q) (eval-expo '(car (quote (a b c))) '() q))
;;   '(a))

;; (test "13"
;;   (run* (q) (eval-expo '(cdr (quote (a b c))) '() q))
;;   '((b c)))

;; (test "14a"
;;   (run* (q) (eval-expo '(pair? (car (quote (a b c)))) '() q))
;;   '(#f))

;; (test "14b"
;;   (run* (q) (eval-expo '(pair? (cdr (quote (a b c)))) '() q))
;;   '(#t))

;; (test "14c"
;;   (run* (q) (eval-expo '(pair? (quote ())) '() q))
;;   '(#f))

;; (test "15a"
;;   (run* (q) (eval-expo '(null? (cdr (quote (a b c)))) '() q))
;;   '(#f))

;; (test "15b"
;;   (run* (q) (eval-expo '(null? (cdr (cdr (quote (a b c))))) '() q))
;;   '(#f))

;; (test "15c"
;;   (run* (q) (eval-expo '(null? (cdr (cdr (cdr (quote (a b c)))))) '() q))
;;   '(#t))

;; (test "15d"
;;   (run* (q) (eval-expo '(null? (quote ())) '() q))
;;   '(#t))

;; (test "16"
;;   (run* (q) (eval-expo '(and 5 6) '() q))
;;   '(6))

;; (test "17"
;;   (run* (q) (eval-expo '(and #f 6) '() q))
;;   '(#f))

;; (test "18a"
;;   (run* (q) (eval-expo '(and (and (and (and (null? (quote ())) 7) 8) 9) 6) '() q))
;;   '(6))

;; (test "18b"
;;   (run* (q) (eval-expo '(and 6 (and (and (and 7 (null? (quote ()))) 8) 9)) '() q))
;;   '(9))

;; (test "19"
;;   (run* (q) (eval-expo '(and 6 (and (and (and 7 (null? (quote 5))) 8) 9)) '() q))
;;   '(#f))

;; (test "20"
;;   (run* (q) (eval-expo '(if (and 6 (and (and (and 7 (null? (quote 5))) 8) 9))
;;                             (quote true)
;;                             (quote false))
;;                        '()
;;                        q))
;;   '(false))

;; (test "21"
;;   (run* (q) (eval-expo '(if (and 6 (and (and (and 7 (null? (quote ()))) 8) 9))
;;                             (quote true)
;;                             (quote false))
;;                        '()
;;                        q))
;;   '(true))


#!eof

(test "twines-1"
  (run 1 (out)
    (fresh (dog cat)
      (=/= dog cat)
      (evalo dog cat)
      (evalo cat dog)
      (== `(,dog ,cat) out)))
  '((('((lambda (_.0)
          (list 'quote (list _.0 (list 'quote _.0))))
        '(lambda (_.0)
           (list 'quote (list _.0 (list 'quote _.0)))))
      ((lambda (_.0)
         (list 'quote (list _.0 (list 'quote _.0))))
       '(lambda (_.0)
          (list 'quote (list _.0 (list 'quote _.0))))))
     (=/= ((_.0 closure)) ((_.0 int-val)) ((_.0 list))
          ((_.0 quote)))
     (sym _.0))))

(test "thrines-1"
  (run 1 (out)
    (fresh (dog cat mouse)
      (=/= dog cat)
      (=/= dog mouse)
      (=/= cat mouse)
      (evalo dog cat)
      (evalo cat mouse)
      (evalo mouse dog)
      (== `(,dog ,cat ,mouse) out)))
  '(((''((lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))) '(lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
      '((lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))) '(lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
      ((lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))) '(lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))))
     (=/= ((_.0 closure)) ((_.0 int-val)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))


;; (test "append-1"
;;   (run* (q)
;;     (evalo '(letrec ([append
;;                       (lambda (l s)
;;                         (cond
;;                           [(null? l) s]
;;                           [else (cons (car l) (append (cdr l) s))]))])
;;               (append '(a b c) '(d e)))
;;            q))
;;   '(a b c d e))

;; (test "append-2"
;;   (run* (q) (evalo
;;              '((letrec ([append
;;                          (lambda (l s)
;;                            (cond
;;                              [(null? l) s]
;;                              [else (cons (car l) (append (cdr l) s))]))])
;;                  append)
;;                '(a b c) '(d e))
;;              q))
;;   '(a b c d e))

;; if-versions of append

#|
(test "if-append-1"
  (run* (q)
    (evalo '(letrec ([append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))])
              (append '(a b c) '(d e)))
           q))
  '(a b c d e))

(test "if-append-2"
  (run* (q) (evalo
             '((letrec ([append
                         (lambda (l s)
                           (if (null? l)
                               s
                               (cons (car l) (append (cdr l) s))))])
                 append)
               '(a b c) '(d e))
             q))
  '(a b c d e))
|#

;; ugly factorial
(define rel-fact5
  `((lambda (f)
      ((f f) (int-exp ,(build-num 5))))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            (int-exp ,(build-num 1))
            (* n ((f f) (sub1 n))))))))

(test "appA-5"
  (run* (q) (evalo rel-fact5 q))
  '((int-val (0 0 0 1 1 1 1))))


(test "appA-3"
  (length (run 500 (q) (evalo q `(int-val ,(build-num 6)))))
  500)


(test "appA-1"
  (run 12 (q) (evalo q `(int-val ,(build-num 6))))
  '((int-exp (0 1 1))
    (sub1 (int-exp (1 1 1)))
    ((lambda () (int-exp (0 1 1))))
    (((lambda (_.0) (int-exp (0 1 1))) '_.1)
     (=/= ((_.0 int-exp)))
     (absento (closure _.1) (int-val _.1)))
    (* (int-exp (1)) (int-exp (0 1 1)))
    (* (int-exp (0 1 1)) (int-exp (1)))
    (* (int-exp (0 1)) (int-exp (1 1)))
    (car (list (int-exp (0 1 1))))
    (((lambda (_.0 _.1) (int-exp (0 1 1))) '_.2 '_.3)
     (=/= ((_.0 int-exp)) ((_.1 int-exp)))
     (absento (closure _.2) (closure _.3) (int-val _.2)
              (int-val _.3)))
    ((lambda () (sub1 (int-exp (1 1 1)))))
    (* (int-exp (1 1)) (int-exp (0 1)))
    ((lambda () ((lambda () (int-exp (0 1 1)))))))) 
