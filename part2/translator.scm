(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm")

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-senv)))))))

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv)
      (cases expression exp
        
        (const-exp (num) (const-exp num))
        
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)))
        
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv)))
        
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)
            (translation-of exp3 senv)))
        
        (var-exp (var)
         ; ################################################
         ; ############ implement translation of var-exp here
         ; ################################################
           ;; this is the format: #(struct:var-exp x1)
         ;; var apply-senv(senv var)this gives the right lexical address
         ;; (map display (list var (nameless-var-exp
         ;;  (apply-senv-number senv var)))) x has been reinitialized. x2 created and shadows x1

                 
                 
          (let ((numStr (number->string (- (apply-senv-number senv var) 1))))
             (let ((name (string->symbol (string-append (symbol->string var) numStr))))
               (let ((new-senv (extend-senv var senv)))
                   (var-exp name)))) 
         )
        
        (let-exp (var exp1 body)
          ; ################################################
          ; ############ implement translation of let-exp here
          ; ################################################
          ;(let ((numStr (number->string (apply-senv-number senv var))))
          ;  (let ((name (string->symbol (string-append (symbol->string var) numStr))))
          ;    (let ((new-senv (extend-senv var senv)))
          ;      (if (> (string->number numStr) 1)
          ;           (begin
          ;             (newline)
          ;             (display var)
          ;             (display " has been reinitialized. ")
          ;             (display name)
          ;             (display " created and shadows ")
          ;             (display (string->symbol (string-append (symbol->string var) (number->string (- (string->number numStr) 1)))))
          ;             (newline)
          ;             (let-exp
          ;              name
          ;              (translation-of exp1 new-senv)
          ;              (translation-of body new-senv)))
          ;           (let-exp
          ;             name
          ;             (translation-of exp1 new-senv)
          ;             (translation-of body new-senv))))))
          
          (let ((numStr (number->string (apply-senv-number senv var))))
            (let ((name (string->symbol (string-append (symbol->string var) numStr))))
              (let ((new-senv (extend-senv var senv)))
                (if (> (string->number numStr) 1)
                    (let ((shadow-name (string-append (symbol->string name) " " (symbol->string var) " has been reinitialized. " (symbol->string name) " created and shadows " (symbol->string var) (number->string (- (string->number numStr) 1)))))
                       (let-exp
                        (string->symbol shadow-name)
                        (translation-of exp1 new-senv)
                        (translation-of body new-senv)))
                     (let-exp
                       name
                       (translation-of exp1 new-senv)
                       (translation-of body new-senv))))))
                   
         )
        
        (proc-exp (var body)
         ; ################################################
         ; ############ implement translation of proc-exp here
         ; ################################################

          (let ((numStr (number->string (apply-senv-number senv var))))
            (let ((name (string->symbol (string-append (symbol->string var) numStr))))
              (let ((new-senv (extend-senv var senv)))
                (if (> (string->number numStr) 1)
                    (let ((shadow-name (string-append (symbol->string name) " " (symbol->string var) " has been reinitialized. " (symbol->string name) " created and shadows " (symbol->string var) (number->string (- (string->number numStr) 1)))))
                       (proc-exp
                        (string->symbol shadow-name)
                        (translation-of body new-senv)))
                (proc-exp
                 name
                 (translation-of body new-senv))))))
                
         )
        
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv)
            (translation-of rand senv)))
        (else (report-invalid-source-expression exp))
        )))

  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp)))
  
   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;
  
  ;;; Senv = Listof(Sym)
  ;;; Lexaddr = N

  ;; empty-senv : () -> Senv
  ;; Page: 95
  (define empty-senv
    (lambda ()
      '()))

  ;; extend-senv : Var * Senv -> Senv
  ;; Page: 95
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))

  (define apply-senv-number
  ; ###########################################################
  ; ###### define apply-senv-number, a procedure that applies
  ; ###### the environment and finds the occurences of variable
  ; ###### var in the environment senv
  ; ###########################################################
    (lambda (senv var)
      (cond
        ((null? senv) 1)
        ((eqv? var (car senv))
         (+ 1 (apply-senv-number (cdr senv) var)))
        (else
          (+ 0 (apply-senv-number (cdr senv) var)))))
   )
  
  ;; apply-senv : Senv * Var -> Lexaddr
  ;; Page: 95
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
          (+ 1 (apply-senv (cdr senv) var))))))

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var)))

  ;; init-senv : () -> Senv
  ;; Page: 96
  (define init-senv
    (lambda ()
      (extend-senv 'i
        (extend-senv 'v
          (extend-senv 'a
            (empty-senv))))))
  
  )
