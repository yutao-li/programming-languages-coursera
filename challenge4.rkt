#lang racket
(provide (all-defined-out))

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

(define (remove-lets-and-pairs e)
  (cond [(add? e) 
         (let ([v1 (remove-lets-and-pairs (add-e1 e))]
               [v2 (remove-lets-and-pairs (add-e2 e))])
           (add v1 v2))]
        ;; CHANGE add more cases here
        [(closure? e) (closure (closure-env e) (remove-lets-and-pairs (closure-fun e)))]
        [(fun? e) (fun (fun-nameopt e) (fun-formal e) (remove-lets-and-pairs (fun-body e)))]
        [(ifgreater? e)
         (let ([e1 (remove-lets-and-pairs (ifgreater-e1 e))]
               [e2 (remove-lets-and-pairs (ifgreater-e2 e))]
               [e3 (remove-lets-and-pairs (ifgreater-e3 e))]
               [e4 (remove-lets-and-pairs (ifgreater-e4 e))])
           (ifgreater e1 e2 e3 e4))]
        [(mlet? e)
         (let ([exp (remove-lets-and-pairs (mlet-e e))]
               [body (remove-lets-and-pairs (mlet-body e))])
           (call (fun #f (mlet-var e) body) exp))]
        [(apair? e) (fun #f "_f" (call (call (var "_f") (mlet "_x" (remove-lets-and-pairs (apair-e1 e)) (var "_x")))
                                       (mlet "_x" (remove-lets-and-pairs (apair-e2 e))(var "_x"))))]
        [(fst? e) (let ([pair (remove-lets-and-pairs (fst-e e))])
                    (call pair (fun #f "x" (fun #f "y" (var "x")))))]
        [(snd? e) (let ([pair (remove-lets-and-pairs (snd-e e))])
                    (call pair (fun #f "x" (fun #f "y" (var "y")))))]
        [(isaunit? e) (isaunit (remove-lets-and-pairs (isaunit-e e)))]
        [(call? e) (call (remove-lets-and-pairs (call-funexp e)) (remove-lets-and-pairs (call-actual e)))]
        [#t e]))

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rls)
  (if (null? rls)
      (aunit)
      (apair (car rls) (racketlist->mupllist (cdr rls)))))

(define (mupllist->racketlist  mls)
  (if (aunit? mls)
      null
      (cons (apair-e1 mls) (mupllist->racketlist (apair-e2 mls)))))
                    
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(closure? e) e]
        [(fun? e) (let ([funcname (fun-nameopt e)]
                        [argname (fun-formal e)])
                    (if (not (string? argname))
                        (error "not argname")
                        (if (or (not funcname) (string? funcname))
                            (closure env e)
                            (error "not funcname"))))]
        [(int? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([num1 (eval-under-env (ifgreater-e1 e) env)]
               [num2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? num1) (int? num2))
               (if (> (int-num num1) (int-num num2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "not number")))]
        [(mlet? e)
         (let ([varname (mlet-var e)])
           (if (not (string? varname))
               (error "not varname")
               (let ([value (eval-under-env (mlet-e e) env)])
                 (eval-under-env (mlet-body e) (cons (cons varname value) env)))))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([pair (eval-under-env (fst-e e) env)])
                    (if (not (apair? pair))
                        (error "not pair")
                        (apair-e1 pair)))]
        [(snd? e) (let ([pair (eval-under-env (snd-e e) env)])
                    (if (not (apair? pair))
                        (error "not pair")
                        (apair-e2 pair)))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                          (int 1)
                          (int 0))]
        [(call? e) (let ([clo (eval-under-env (call-funexp e) env)])
                     (if (not (closure? clo))
                         (error "not closure")
                         (let* ([arg (eval-under-env (call-actual e) env)]
                                [fun (closure-fun clo)]
                                [funcbody (fun-body fun)]
                                [argname (fun-formal fun)]
                                [funcname (fun-nameopt fun)]
                                [curenv (cons (cons argname arg) (closure-env clo))]
                                [curenv (if funcname
                                            (cons (cons funcname clo) curenv)
                                            curenv)])
                           (eval-under-env funcbody curenv))))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env (remove-lets-and-pairs e) null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y" ) (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "fun"
       (fun "mapf" "ls"
            (ifaunit (var "ls")
                     (aunit)
                     (apair (call (var "fun") (fst (var "ls"))) (call (var "mapf") (snd (var "ls"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n"
             (fun #f "ls"
                  (call (call (var "map") (fun #f "i" (add (var "n") (var "i")))) (var "ls"))))))
(eval-exp (call (call mupl-map (fun #f "x" (var "x"))) (apair (int 1) (aunit))))

(closure
 '()
 (fun
  #f
  "_f"
  (call
   (call
    (var "_f")
    (mlet
     "_x"
     (call
      (var "fun")
      (call (var "ls") (fun #f "x" (fun #f "y" (var "x")))))
     (var "_x")))
   (mlet
    "_x"
    (call
     (var "mapf")
     (call (var "ls") (fun #f "x" (fun #f "y" (var "y")))))
    (var "_x")))))

(closure
 '()
 (fun
  #f
  "_f"
  (call (call (var "_f") (mlet "_x" (int 1) (var "_x"))) (mlet "_x" (aunit) (var "_x")))))
