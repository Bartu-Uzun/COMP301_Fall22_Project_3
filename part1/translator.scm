(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm")
  (require "environments.scm")
  (require "data-structures.scm")  ;Extra Change

  (provide translation-of-program)

  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-env)))))))

  (define translation-of
    (lambda (exp env)
      (cases expression exp
        
        (const-exp (num) (const-exp num))
        
        (var-exp (var) (var-exp var))
        
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 env)
            (translation-of exp2 env)))
        
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 env)))
        
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 env)
            (translation-of exp2 env)
            (translation-of exp3 env)))
       
        (let-exp (var exp1 body)
          (let-exp
             var
            (translation-of exp1 env)            
            (translation-of body env)))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### THE LINES BELOW ARE PUT ONLY TO PROVIDE YOU
        ; ###### A WORKING CODE BASELINE. THEY ARE REQUIRED
        ; ###### TO CHANGE.
        ; ######
        ; ###### Here, you need to translate the original
        ; ###### proc-exp, call-exp, and letrec-exp to the 
        ; ###### nested versions. To understand how translation
        ; ###### works, you can check the lexaddr code in the
        ; ###### EOPL GUI. 
        ; ###### In this part, you will also do the count 
        ; ###### incrementation!!!
        ; #####################################################
        
        ; #####################################################
        ; ###### proc-nested-exp has variable name, the count, anonym and the body as arguments
        ; ##################################################### (proc-nested-exp (b-var count p-name p-body)
        (proc-exp (var body)
                  
          (proc-nested-exp
           var
           'count
           'anonym
           (translation-of body env)
           )
          
        )
        ; #####################################################
        ; ###### call-nested-exp has operator, operand and the count as arguments
        ; ###### if the operator is an var-exp, it means that it is already
        ; ###### in the environment therefore count will be incremented,
        ; ###### else it will be initialized as 1
        ; ###### Hint: for incrementing, you can use diff-exp
        ; #####################################################
        (call-exp (rator rand)

           (cases expression rator
             (var-exp (var)
               (begin
               (call-nested-exp
                (translation-of rator env)
                (translation-of rand env)
                (diff-exp (var-exp 'count) (const-exp -1))) ;get count value from env, add 1 to it
                ))
               (else
                (call-nested-exp
                 (translation-of rator env)
                 (translation-of rand env)
                 (const-exp 1) ;initialize count as 1
                 )))

          
        )
        ; #####################################################
        ; ###### count should be included in the nested version
        ; ##################################################### (letrec-nested-exp (p-name b-var b-count p-body letrec-body)
        (letrec-exp (p-name b-var p-body letrec-body)

          (letrec-nested-exp
           p-name
           b-var
           'count
           (translation-of p-body env)
           (translation-of letrec-body env)
           )
         
        )

        ; #####################################################
        
        (else (report-invalid-source-expression exp))
        )))
  
 (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp)))
)
  