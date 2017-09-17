#lang play
(require "base.rkt")

;;tests para emptyT-env
(test emptyT-env mtTEnv)

;;tests para extendT-env
(test (extendT-env 'a (TNum) emptyT-env) (anTEnv 'a (TNum) mtTEnv))

(test (extendT-env 'a (TVar 1) (extendT-env 'b (TVar 2)
                                            (extendT-env 'c (TVar 3)
                                                         emptyT-env)))
      (anTEnv 'a (TVar 1) (anTEnv 'b (TVar 2) (anTEnv 'c (TVar 3)
                                                      mtTEnv))))


;;tests para lookupT-env
(test/exn (lookupT-env 'y (emptyT-env)) "Exception: free identifier y")

(test (lookupT-env 'a (extendT-env 'a (TNum) emptyT-env))
      (TNum))

(test (lookupT-env 'c (extendT-env 'a (TVar 1) (extendT-env 'b (TVar 2)
                                            (extendT-env 'c (TVar 3)
                                                         emptyT-env))))
      (TVar 3))


;;tests para typeof
(test (typeof(if0 (sub (num 5) (num 5)) (if0 (sub (add (num 5) (num 3)) (num 8))
                                  (num 1) (num 4)) (num 4)) emptyT-env)
      (list
 (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))
 (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))
 (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))
 (Cnst (TNum) (TNum))))

(test (typeof  (num 3) (emptyT-env)) (list (TNum)))

(test (typeof  (add (num 10) (num 3)) (emptyT-env))
      (list (TNum)(Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))

(test (typeof  (if0 (num 2) (num 5) (num 3)) (emptyT-env))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))

(test (typeof  (id 'x) (extendT-env 'x (TNum) (emptyT-env))) (list (TNum)))

(test (typeof  (add (num 10) (id 'x)) (extendT-env 'x (TVar 1) (emptyT-env)))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum))))

(test (typeof  (app (fun 'x (id 'x)) (num 3)) (emptyT-env))
      (list (TVar 1) (Cnst (TFun (TVar 2) (TVar 2)) (TFun (TNum) (TVar 1)))))

(test (typeof  (fun 'x (add (id 'x) (num 1))) (emptyT-env))
      (list (TFun (TVar 3) (TNum)) (Cnst (TVar 3) (TNum))
            (Cnst (TNum) (TNum))))

(test (typeof  (fun 'f (fun 'x (app (id 'f) (app (id 'f) (id 'x)))))
               (emptyT-env)) (list (TFun (TVar 4) (TFun (TVar 5) (TVar 6)))
                                   (Cnst (TVar 4) (TFun (TVar 5) (TVar 7)))
                                   (Cnst (TVar 4) (TFun (TVar 7) (TVar 6)))))


;;tests para equalTVar
(test (equalTVar (TVar 1) (TVar 2)) #f)
(test (equalTVar (TVar 3) (TVar 3)) #t)


;;tests para equal-t-f
(test (equal-t-f (TFun (TVar 5) (TFun (TVar 3) (TVar 5))) (TVar 5) (TNum))
      (TFun (TNum) (TFun (TVar 3) (TNum))))

(test (equal-t-f (TFun (TVar 5) (TFun (TVar 3) (TVar 5))) (TVar 1) (TNum))
      (TFun (TVar 5) (TFun (TVar 3) (TVar 5))))

(test (equal-t-f (TFun (TVar 5) (TFun (TVar 3) (TFun (TVar 1)
      (TFun (TVar 3) (TVar 5))))) (TVar 5) (TNum))
      (TFun (TNum) (TFun (TVar 3) (TFun (TVar 1) (TFun (TVar 3) (TNum))))))


;;tests para equal-c-f
(test (equal-c-f (Cnst (TFun (TVar 1) (TFun (TVar 5) (TVar 1)))
                       (TFun (TVar 1) (TNum))) (TVar 1) (TVar 6))
      (Cnst (TFun (TVar 6) (TFun (TVar 5) (TVar 6)))
                       (TFun (TVar 6) (TNum))))

(test (equal-c-f (Cnst (TFun (TVar 1) (TFun (TVar 5) (TVar 1)))
                       (TFun (TVar 1) (TNum))) (TVar 8) (TVar 6))
      (Cnst (TFun (TVar 1) (TFun (TVar 5) (TVar 1)))
                       (TFun (TVar 1) (TNum))))


;;tests para substitute
(test (substitute (TVar 1) (TNum) '()) '())

(test (substitute (TVar 4) (TFun (TNum) (TNum)) (list
                                (Cnst (TVar 1) (TFun (TVar 2) (TVar 4)))
            (Cnst (TVar 1) (TFun (TVar 4) (TFun (TVar 4) (TNum))))))
      (list (Cnst (TVar 1) (TFun (TVar 2) (TFun (TNum) (TNum))))
            (Cnst (TVar 1) (TFun (TFun (TNum) (TNum))
                                 (TFun (TFun (TNum) (TNum)) (TNum)))))
      )


;;tests para occurs-in?
(test (occurs-in? (TVar 5) (TNum)) #f)

(test (occurs-in? (TVar 5) (TFun (TVar 1) (TFun (TVar 4) (TNum)))) #f)

(test (occurs-in? (TVar 4) (TFun (TVar 1) (TFun (TVar 4) (TNum)))) #t)

(test (occurs-in? (TVar 5) (TFun (TVar 1) (TFun (TVar 4)
      (TFun (TVar 4) (TFun (TVar 5) (TVar 5)))))) #t)


;;tests para equalT?
(test (equalT? (TVar 4) (TVar 3)) #f)

(test (equalT? (TNum) (TFun (TNum) (TNum))) #f)

(test (equalT? (TFun (TNum) (TVar 5)) (TFun (TNum) (TVar 5))) #t)

(test (equalT? (TFun (TNum) (TVar 5)) (TFun (TNum) (TVar 4))) #f)

(test (equalT? (TFun (TNum) (TFun (TVar 1) (TFun (TVar 2) (TNum))))
               (TFun (TNum) (TFun (TVar 1) (TFun (TVar 2) (TVar 2))))) #f)

;;tests para isTVar
(test (isTVar (TVar 5)) #t)
(test (isTVar (TFun (TVar 1) (TVar 2))) #f)


;;tests para unify
(test (unify '()) '())

(test (unify (list (Cnst (TFun (TVar 2) (TVar 2)) (TFun (TNum) (TVar 1)))))
      (list (Cnst (TVar 1) (TNum)) (Cnst (TVar 2) (TNum))))

(test (unify (list (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TNum))))
      (list (Cnst (TVar 1) (TNum))))

(test (unify (list (Cnst (TVar 1) (TFun (TVar 2) (TVar 4)))
                   (Cnst (TVar 1) (TFun (TVar 4) (TVar 3)))))
      (list (Cnst (TVar 4) (TVar 3)) (Cnst (TVar 2) (TVar 4))
            (Cnst (TVar 1) (TFun (TVar 2) (TVar 4)))))

(test/exn (unify (list (Cnst (TNum) (TNum))
               (Cnst (TNum) (TNum))
               (Cnst (TVar 1) (TFun (TVar 2)
    (TFun (TVar 1) (TFun (TVar 3) (TNum))))))) "Exception: Type error: cannot unify (TVar 1) with (TFun (TVar 2) (TFun (TVar 1) (TFun (TVar 3) num)))")


;;tests para auxRun
(test (auxRun (TFun (TVar 3) (TFun (TVar 4) (TVar 5)))
          (list (Cnst (TNum) (TNum))
 (Cnst (TVar 3) (TNum))
 (Cnst (TNum) (TFun (TVar 4) (TVar 6)))
 (Cnst (TVar 3) (TFun (TVar 6) (TVar 5))))
          (list (Cnst (TNum) (TNum))
 (Cnst (TVar 3) (TNum))
 (Cnst (TNum) (TFun (TVar 4) (TVar 6)))
 (Cnst (TVar 3) (TFun (TVar 6) (TVar 5)))))
      (TFun (TNum) (TFun (TVar 4) (TVar 5))))

(test (auxRun (TNum) '() '()) (TNum))



;;tests para runType
(test (runType '(fun (x) (+ (+ x (+ 1 3)) 1))) (TFun (TNum) (TNum)))

(test (runType '(fun (x) (+ x 1))) (TFun (TNum) (TNum)))

(test (runType '(fun (x) x)) (TFun (TVar 10) (TVar 10)))

(test (runType '(fun (x) 3)) (TFun (TVar 11) (TNum)))

(test/exn (runType 'x) "Exception: free identifier x")

(test/exn (runType '((fun (x) (+ x 1)) (fun (x) x)))
          "Exception: Type error: cannot unify num with (TFun (TVar 14) (TVar 14))")

(test (runType '(fun (f) (fun (x) (f (f x)))))
      (TFun (TFun (TVar 17) (TVar 17)) (TFun (TVar 17) (TVar 17))))