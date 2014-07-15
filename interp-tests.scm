(load "interp.scm")
(load "test-check.scm")

;; TODO
;;
;; add factorial test
;;
;; add unit tests for cons, cdr, letrec, cond, etc
;;
;; add twines and thrines


(test "pluso-1"
  (run 5 (q)
    (fresh (x y z)
      (pluso x y z)
      (== `(,x ,y ,z) q)))
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


(test "quines-1"
  (run 5 (q) (evalo q q))
  '(#t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0))) '(lambda (_.0) (list _.0 (list 'quote _.0)))) (=/= ((_.0 closure)) ((_.0 int-val)) ((_.0 list)) ((_.0 quote))) (sym _.0))
    (((lambda (_.0) (list (list 'lambda '(_.0) _.0) (list 'quote _.0))) '(list (list 'lambda '(_.0) _.0) (list 'quote _.0))) (=/= ((_.0 closure)) ((_.0 int-val)) ((_.0 list)) ((_.0 quote))) (sym _.0))
    (((lambda (_.0) (list _.0 (list (car '(quote . _.1)) _.0))) '(lambda (_.0) (list _.0 (list (car '(quote . _.1)) _.0)))) (=/= ((_.0 car)) ((_.0 closure)) ((_.0 int-val)) ((_.0 list)) ((_.0 quote))) (sym _.0) (absento (closure _.1) (int-val _.1)))))

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
    ((lambda () (int-exp (0 1 1))))
    (sub1 (int-exp (1 1 1)))
    (((lambda (_.0) (int-exp (0 1 1))) '_.1) (=/= ((_.0 int-exp))) (absento (closure _.1) (int-val _.1)))
    (* (int-exp (1)) (int-exp (0 1 1)))
    (* (int-exp (0 1 1)) (int-exp (1)))
    (* (int-exp (0 1)) (int-exp (1 1)))
    (((lambda (_.0) (int-exp (0 1 1))) (list)) (=/= ((_.0 int-exp))))
    (car (list (int-exp (0 1 1))))
    ((lambda () ((lambda () (int-exp (0 1 1))))))
    (sub1 ((lambda () (int-exp (1 1 1)))))
    ((lambda () (sub1 (int-exp (1 1 1))))))) 
