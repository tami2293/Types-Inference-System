#lang play

;################################ Interprete visto en clases ###########################

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg))


;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs where
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n)]
    [ x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]))


;; Abstract Dada Type (ADT) for handling environments 
;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))


;; Values of expressions 
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (numV n)
  (closureV id body env))

;; Auxiliary functions handling numeric Values
(define (op-bin f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(define (op-un f n)
  (numV (f (numV-n n))))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (op-bin + (eval l env) (eval r env))]
    [(sub l r) (op-bin - (eval l env) (eval r env))]
    [(if0 c t f) (if  (op-un zero? (eval c env))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (closureV the-arg the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]))


;; run :: s-expr -> Value
(define (run prog)
  (eval (parse prog) (mtEnv)))





;################################ Definiciones ###########################

(deftype Type
  (TNum)
  (TFun Targ Tret)
  (TVar Symbol))

(deftype Constraint
  (Cnst T1 T2))

(deftype TEnv
  (mtTEnv)
  (anTEnv id Type env))

(define count 0)

(define (get-id)
  (begin
    (set! count (add1 count))
    count))

(define (reset)
  (set! count 0))

(define (prettyfy T)
  (match T
    [(TNum) "num"]
    [(TVar x) (string-append "(TVar "(number->string x) ")")]
    [(TFun T1 T2) (string-append "(TFun " (prettyfy T1) " " (prettyfy T2) ")")]))




;################################ Su código va aquí ###########################

;; emptyT-env :: TEnv
;; Creates an empty environment of types.
(define emptyT-env
  mtTEnv)

;; extendT-env :: Sym Type TEnv -> TEnv
;; Extends an environment associating a type to an id.
(define extendT-env anTEnv)

;; lookupT-env :: Sym TEnv -> Type
;; Returns the type associated to an id, given the id
;; and the types environment.
(define (lookupT-env x env)
  (match env
    [(mtTEnv) (error "Exception: free identifier" x)]
    [(anTEnv id val rest) (if (symbol=? id x)
                            val
                            (lookupT-env x rest))]))

;; typeof :: Expr TEnv -> (Type, List[Constraint])
;; Returns the type of an expression and the constraints list that must be 
;; solved for the program to be valid.
(define (typeof expr env)
  (match expr
    [(num n) (list (TNum))]
    [(add e1 e2) (append(list (TNum)) (cdr(typeof e1 env)) (cdr(typeof e2 env))
                        (list (Cnst (car(typeof e1 env)) (TNum))
                       (Cnst (car(typeof e2 env)) (TNum))) 
                                                           )]
    [(sub e1 e2) (append(list (TNum)) (cdr(typeof e1 env)) (cdr(typeof e2 env))
                        (list (Cnst (car(typeof e1 env)) (TNum))
                       (Cnst (car(typeof e2 env)) (TNum))) 
                                                           )]
    [(if0 e tb fb) (append(list (car(typeof tb env)))
                          (cdr(typeof e env))
                          (cdr(typeof tb env))
                          (cdr(typeof fb env))
                                (list (Cnst (car(typeof e env)) (TNum))
                                (Cnst (car(typeof tb env)) (car(typeof fb env)))))]
    [(id x) (list (lookupT-env x env))]
    [(fun arg body) (let ([new-env (extendT-env arg (TVar (get-id)) env)])
     (let ([new-body (typeof body new-env)])
     (append(list(TFun (car(typeof (id arg) new-env)) (car new-body)))
                           (cdr new-body))))]
    [(app fun arg)
     (let ([appType (TVar (get-id))])
     (let ([funType (typeof fun env)])
     (let ([argType (typeof arg env)])
     (append(list appType)
            (cdr funType)
            (cdr argType)
            (list(Cnst (car funType)
                                  (TFun (car argType) appType)))
            ))))]
   ))

;; equalTVar :: TVAR TVAR -> bool
;; Compares the equality between two type variables, and returns the
;; boolean value of this comparation.
(define (equalTVar tvar1 tvar2)
  (match tvar1
    [(TVar n1) (match tvar2
                [(TVar n2) (equal? n1 n2)])]
    ))

;; equal-t-f :: Type TVAR Type -> Type
;; Replaces a type variable by a given type inside a types expression
;; (another type given) and returns this new types expression.
(define (equal-t-f type from to)
  (match type
    [(TVar n) (if (equalTVar type from)
                  to
                  type)]
    [(TNum) (TNum)]
    [(TFun A B) (TFun (equal-t-f A from to) (equal-t-f B from to))]
   ))

;; equal-c-f :: Constraint TVAR Type -> Constraint
;; Replaces a type variable by a given type inside a constraint and returns
;; the new constraint.
(define (equal-c-f cnst from to)
  (match cnst
    [(Cnst A B) (Cnst (equal-t-f A from to) (equal-t-f B from to))]))

;; substitute :: TVAR Type List[Constraint] -> List[Constraint]
;; Replaces a variable type by another given type inside a list of constraints
;; and returns the new list.
(define (substitute from to _list)
  (if (empty? _list)
      _list
      (append (list (equal-c-f (car _list) from to))
              (substitute from to (cdr _list)))
   )
 )

;; occurs-in? :: TVAR Type -> bool
;; Verifies if a type variable occurs as a sub-expression of another
;; given type.
(define (occurs-in? tvar t)
  (match t
    [(TVar n) (equalTVar tvar t)]
    [(TNum) #f]
    [(TFun T1 T2) (or (occurs-in? tvar T1) (occurs-in? tvar T2))]
  ))

;; equalT? :: Type Type -> bool
;; Compares the equality between two type expressions and returns the boolean
;; value of this comparation.
(define (equalT? t1 t2)
  (match t1
    [(TNum) (match t2
              [(TNum) #t]
              [(TVar n) #f]
              [(TFun A B) #f]
              )]
    [(TVar n1) (match t2
                [(TVar n2) (equal? n1 n2)]
                [(TNum) #f]
                [(TFun A B) #f]
                 )]
    [(TFun A1 B1) (match t2
                  [(TNum) #f]
                  [(TVar n) #f]
                  [(TFun A2 B2) (and (equalT? A1 A2) (equalT? B1 B2))]
                    )]
   ))

;; isTVar :: Type -> bool
;; Verifies if a given type is a type variable.
(define (isTVar t)
  (match t
  [(TVar n) #t]
  [(TNum) #f]
  [(TFun A B) #f]
    ))

;; unify :: List[Constraint] -> List[Constraint]
;; Returns the unification (a unified constraints list) of a given
;; contraints list. 
(define (unify _list)
  (if (empty? _list)
      empty
      (let ([theHead (car _list)])
      (let ([theTail (cdr _list)])
       (match theHead
         [(Cnst T1 T2) (if (equalT? T1 T2)
            (unify theTail)
            (if (and (isTVar T1) (not (occurs-in? T1 T2)))
                (append (unify(substitute T1 T2 theTail)) (list(Cnst T1 T2)))
                (if (and (isTVar T2) (not (occurs-in? T2 T1)))
                    (append (unify(substitute T2 T1 theTail)) (list(Cnst T2 T1)))
                                   (match T1
                                     [(TFun T1a T1r)
                                      (match T2
                                        [(TFun T2a T2r)
                                         (unify
                                          (append theTail (list(Cnst T1a T2a)
                                                  (Cnst T1r T2r))))]
                                        [(TNum) (error (string-append "Exception: Type error: cannot unify " (prettyfy (TNum)) " with " (prettyfy T1)))]
                                        [(TVar n) (error (string-append "Exception: Type error: cannot unify " (prettyfy T2) " with " (prettyfy T1)))]
                                                      )]
                                     [(TNum) (error (string-append "Exception: Type error: cannot unify " (prettyfy (TNum)) " with " (prettyfy T2)))]
                                     [(TVar n) (error (string-append "Exception: Type error: cannot unify " (prettyfy T1) " with " (prettyfy T2)))]
                                     ))))])))))

;; auxRun :: Type List[Constraint] List[Constraint] -> Type
;; Looks for and returns the final type of a given type using its
;; unified constraints list. cnstList and in-cnstList must be the
;; same list.
(define (auxRun type cnstList in-cnstList)
  (match type
    [(TNum) type]
    [(TFun A B) (TFun (auxRun A cnstList in-cnstList) (auxRun B cnstList in-cnstList))]
    [(TVar n) (if (empty? cnstList)
                  type
                  (match (car cnstList)
                    [(Cnst T1 T2)
                     (if (and (isTVar T1) (equalTVar type T1))
                     (auxRun T2 in-cnstList in-cnstList)
                     (auxRun type (cdr cnstList) in-cnstList))
                     ])
                  )]
                
  ))

;; runType :: S-expr -> Type
;; Returns the final type of an expression.
(define (runType s-expr)
  (let ([type-of (typeof (parse s-expr) (emptyT-env))])
    (let ([unified (unify (cdr type-of))])
      (auxRun (car type-of) unified unified)
      )))

