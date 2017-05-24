;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  label generator ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^^label
  (lambda (prefix)
    	(let ((n 0))
    		(lambda ()
    			(set! n (+ n 1))
    			  	(string-append
    			  		prefix
    			  		  (number->string n))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-GUL labels    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-code-gen-GUL-next (^^label "L_void_jump_to_next_label_"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-generator         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen
  (lambda (pe major)
   
    (cond
     ((equal? (car pe) 'const)            (code-gen-const pe))
     ((equal? (car pe) 'pvar)             (code-gen-pvar pe))
     ((equal? (car pe) 'bvar)             (code-gen-bvar pe))
     ((equal? (car pe) 'fvar)             (code-gen-fvar pe))
     ((equal? (car pe) 'if3)             (code-gen-if pe major))
     ((equal? (car pe) 'or)               (code-gen-or pe major))
     ((equal? (car pe) 'seq)              (code-gen-seq pe major))
     ((equal? (car pe) 'lambda-simple)    (code-gen-lambda-simple pe    (+ 1 major)))
     ((equal? (car pe) 'lambda-opt)       (code-gen-lambda-opt pe (+ 1 major)))
     ((equal? (car pe) 'lambda-var)  (code-gen-lambda-var pe (+ 1 major)))
     ((equal? (car pe) 'def)           (code-gen-define pe major))
     ((equal? (car pe) 'applic)           (code-gen-applic pe major))
     ((equal? (car pe) 'tc-applic)        (code-gen-tc-applic pe major))
	 ((equal? (car pe) 'set)              (code-gen-set pe major))
	 ((equal? (car pe) 'box)              (code-gen-box pe major))
	 ((equal? (car pe) 'box-get)          (code-gen-box-get pe major)) 
	 ((equal? (car pe) 'box-set)          (code-gen-box-set pe major))



     
     (else (error "unknown expression to handle with: ~s" pe)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-const  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define code-gen-const
	(lambda (pe) 
		
		(let ((const (const->string (get-constant-address (cadr pe)))))
		(string-append
		"MOV(R0, IMM(" const "));\n"

	                 ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-pvar  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-gen-pvar
	(lambda (pe)
	(let ((minor (caddr pe)))
		(string-append
		"MOV(R0,FPARG(" (number->string (+ minor 2) )"));\n"
	                 ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-bvar  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define code-gen-bvar 
  (lambda (pe)
  (with pe
  (lambda (bvar var major minor)
  	(let ((major_s (number->string major))
  	      (minor_s (number->string minor)))
	    (string-append

	     "MOV(R0, FPARG(0));\n"                    
	     "MOV(R0, INDD(R0, "  major_s "));\n"
	     "MOV(R0, INDD(R0, "  minor_s  "));\n"

	     ))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-fvar  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-fvar
	(lambda (pe)
	   (let((free_var (number->string (get-free-var-address (cadr pe)))))
	    (string-append
	     "MOV(R0, IND(" free_var "));\n"
	     ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  if labels       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-if-else (^^label "L_IF_ELSE_"))
(define ^label-if-exit (^^label "L_IF_EXIT_"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-if  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-if
  (lambda (pe major)
    (with pe
	  (lambda (if3 if-test do-if-true-exp do-if-false-exp)
	    (let ((test         (code-gen if-test     major))
		      (do-if-true   (code-gen do-if-true-exp  major))
		      (do-if-false  (code-gen do-if-false-exp major))
		      (L_if-else (^label-if-else))
		      (L-if-exit (^label-if-exit)))
	          (string-append
	                test"\n" 
			       "CMP(R0, SOB_FALSE);\n" 
			       "JUMP_EQ(" L_if-else ");\n" 
			       do-if-true"\n"
			       "JUMP(" L-if-exit ");\n" 
			       L_if-else ":\n" 
			       do-if-false"\n" 
			      L-if-exit ":\n"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  lambda-simple labels       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-lambda-simple-exit (^^label "L_CLOS_EXIT_"))
(define ^label-lambda-simple-code (^^label "L_CLOS_CODE_"))
(define ^label-lambda-simple-env-loop (^^label "L_CLOS_ENV_LOOP_"))
(define ^label-lambda-simple-env-loop-end (^^label "L_CLOS_ENV_LOOP_END_"))
(define ^label-lambda-simple-param-loop (^^label "L_CLOS_PARAM_LOOP_"))
(define ^label-lambda-simple-param-loop-end (^^label "L_CLOS_PARAM_LOOP_END_"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-lambda-simple  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-lambda-simple
  (lambda (pe major)
    (with pe
	  (lambda (lambda-simple args body)
	    (letrec ((code-gen-body  (code-gen body major))
                 (clos-label     (^label-lambda-simple-code))
		         (env-loop-test      (^label-lambda-simple-env-loop))

		         (env-loop       (^label-lambda-simple-param-loop))
		         (env-loop-end   (^label-lambda-simple-env-loop-end))
		         (param-loop     (^label-lambda-simple-param-loop))
		         (param-loop-end (^label-lambda-simple-param-loop-end))
		         (exit-label     (^label-lambda-simple-exit)))                                
	      (string-append

           "MOV(R1,FPARG(0))\n" ;old env
            ;creating new env
	       "PUSH(IMM(" (number->string major) "));\n" 
	       "CALL(MALLOC);\n" ;after malloc R0 pointer to block
	       "DROP(1);\n"
	       "MOV(R2, R0);\n" ;R2 <- pointer to new env
	      ;copy env 
	       "MOV(R4, IMM(1));\n" ;j=1
	       "MOV(R5, IMM(0));\n" ;i=0
	       env-loop ":\n" 
	       "CMP(R4, IMM(" (number->string major) "));\n" 
	       "JUMP_GE(" env-loop-end ");\n" ;jump if i>= major to end of loop
	       ;loop body
	      ; "MOV(INDD(R2, R4), INDD(ENV, R5));\n"
	       "MOV(INDD(R2, R4), INDD(R1, R5));\n" ;R2[j] = R1[i]
           "INCR(R4);\n"  ;j++
	       "INCR(R5);\n"  ;i++
	       "JUMP(" env-loop ");\n" 
	        env-loop-end ":\n" 
            ;creating memory for args. parameters for new env
           "MOV(R3, FPARG(1));\n" ; R3<- n (number of argument)
	       "PUSH(R3);\n"
	       "CALL(MALLOC);\n" 
	       "DROP(1);\n"
	       "MOV(IND(R2), R0);\n" 
	       "MOV(R10,IND(R2));\n"
	       ;IND(R2) = NEW ENV
            ;copy args
	       "MOV(R4, IMM(0));\n" ;i=0
	       ;"MOV(R5, IMM(2));\n" ;j=2 ;OR add
	        param-loop":\n"
	       "CMP(R4, R3);\n"
	       "JUMP_EQ(" param-loop-end ");\n"
	       "MOV(INDD(R10,R4), FPARG(R4+2));\n"
	       "INCR(R4);\n" 
	       "JUMP(" param-loop ");\n"
	       param-loop-end ":\n" 
             ;memory for closure
           "PUSH(IMM(3));\n"
	       "CALL(MALLOC);\n"
	       "DROP(1);\n"              
	       "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"  
	       "MOV(INDD(R0, 1), R2);\n"
	       "MOV(INDD(R0, 2), LABEL(" clos-label"));\n" 
	       "JUMP(" exit-label ");\n"
            ;clousre code
	       clos-label ":\n"
	       "PUSH(FP);\n"
	       "MOV(FP, SP);\n"
		   ;"CMP(FPARG(1),INDD(R0,1));\n"
		   ;"JUMP_NE(L_INCORRECT_NUMBER_OF_ARGS_ERROR);\n"
	       code-gen-body "\n"
	       "POP(FP);\n"
	       "RETURN;\n" 
	       exit-label ":\n" 	       

	       ))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  lambda-opt labels          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-opt-code                (^^label "L_OPT_CLOSURE_CODE_"))
(define ^label-loop-opt-code           (^^label "L_LOOP_OPT_CLOSURE_CODE_"))
(define ^label-loop-end-opt-code       (^^label "L_END_LOOP_OPT_CLOSURE_CODE_"))
(define ^label-opt-env-loop            (^^label "L_OPT_CLOSURE_ENV_LOOP_"))
(define ^label-opt-env-loop-end        (^^label "L_OPT_CLOSURE_ENV_LOOP_END_"))
(define ^label-opt-param-loop          (^^label "L_OPT_CLOSURE_PARAM_LOOP_"))
(define ^label-opt-param-loop-end      (^^label "L_OPT_CLOSURE_PARAM_LOOP_END_"))
(define ^label-loop-1-opt-code         (^^label "L_LOOP_1_OPT_CLOSURE_CODE_"))
(define ^label-loop-1-end-opt-code     (^^label "L_END_LOOP_1_OPT_CLOSURE_CODE_"))
(define ^label-loop-2-opt-code         (^^label "L_LOOP_2_OPT_CLOSURE_CODE_"))
(define ^label-loop-2-end-opt-code     (^^label "L_END_LOOP_2_OPT_CLOSURE_CODE_"))
(define ^label-opt-body-code           (^^label "L_OPT_BODY_CLOSURE_CODE_"))
(define ^label-opt-exit                (^^label "L_OPT_CLOSURE_EXIT_"))
(define ^label-no-opt-code             (^^label "L_NO_ARGS_OPT_CLOSURE_"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-lambda-opt  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-lambda-opt
  (lambda (pe major)
    (with pe
	  (lambda (lambda-opt argl opt body)
	    (letrec ((params-len (length argl))
		     (opt-code-gen-body (code-gen body major))
		     (opt-clos-label         (^label-opt-code))
		     (opt-env-loop           (^label-opt-env-loop))
		     (opt-env-loop-end       (^label-opt-env-loop-end))
		     (opt-arg-loop           (^label-opt-param-loop))
		     (opt-arg-loop-end       (^label-opt-param-loop-end))
             (first-loop-label       (^label-loop-opt-code))
		     (first-loop-label-end   (^label-loop-end-opt-code))
             (second-loop-label      (^label-loop-1-opt-code))
		     (second-loop-label-end  (^label-loop-1-end-opt-code))
		     (third-loop-label       (^label-loop-2-opt-code))
		     (third-loop-label-end   (^label-loop-2-end-opt-code))
		     (opt-body               (^label-opt-body-code))
		     (no-opt-var             (^label-no-opt-code))
		     (opt-exit-label         (^label-opt-exit)))
	      (string-append
	   
       ;closure memory
           "MOV(R1,FPARG(0))\n" ;old env
            ;creating new env
	       "PUSH(IMM(" (number->string major) "));\n" 
	       "CALL(MALLOC);\n" ;after malloc R0 pointer to block
	       "DROP(1);\n"
	       "MOV(R2, R0);\n" ;R2 <- pointer to new env
	      ;copy env 
	       "MOV(R4, IMM(1));\n" ;j=1
	       "MOV(R5, IMM(0));\n" ;i=0
	       opt-env-loop ":\n" 
	       "CMP(R4, IMM(" (number->string major) "));\n" 
	       "JUMP_GE(" opt-env-loop-end ");\n" ;jump if i>= major to end of loop
	       ;loop body
	      ; "MOV(INDD(R2, R4), INDD(ENV, R5));\n"
	       "MOV(INDD(R2, R4), INDD(R1, R5));\n" ;R2[j] = R1[i]
           "INCR(R4);\n"  ;j++
	       "INCR(R5);\n"  ;i++
	       "JUMP(" opt-env-loop ");\n" 
	        opt-env-loop-end ":\n" 
            ;creating memory for args. parameters for new env
           "MOV(R3, FPARG(1));\n" ; R3<- n (number of argument)
	       "PUSH(R3);\n"
	       "CALL(MALLOC);\n" 
	       "DROP(1);\n"
	       "MOV(IND(R2), R0);\n" 
	       "MOV(R10,IND(R2));\n"
	       ;IND(R2) = NEW ENV
            ;copy args
	       "MOV(R4, IMM(0));\n" ;i=0
	       ;"MOV(R5, IMM(2));\n" ;j=2 ;OR add
	        opt-arg-loop":\n"
	       "CMP(R4, R3);\n"
	       "JUMP_EQ(" opt-arg-loop-end ");\n"
	       "MOV(INDD(R10,R4), FPARG(R4+2));\n"
	       "INCR(R4);\n" 
	       "JUMP(" opt-arg-loop ");\n"
	       opt-arg-loop-end ":\n" 
             ;memory for closure
           "PUSH(IMM(3));\n"
	       "CALL(MALLOC);\n"
	       "DROP(1);\n"              
	       "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"  
	       "MOV(INDD(R0, 1), R2);\n"
	       "MOV(INDD(R0, 2), LABEL(" opt-clos-label "));\n" 
	       "MOV(R1,R0);\n"
	       "JUMP(" opt-exit-label ");\n"
            ;clousre code
	       opt-clos-label  ":\n"
	       "PUSH(FP);\n"
	       "MOV(FP, SP);\n"

	       ;reorgeniz stack
	       "CMP(FPARG(1), IMM(" (number->string params-len) "));\n" 
	       "JUMP_LE(" no-opt-var ");\n" 
           "MOV(R0, SOB_NIL);\n" 
	       "MOV(R11, FPARG(1));\n" 
           "DECR(R11);\n" 
	       first-loop-label ":\n" 
	       "CMP(R11, IMM(" (number->string (- params-len 1)) "));\n" 
           "JUMP_EQ(" first-loop-label-end ");\n" 
	       "PUSH(R0);\n" 
	       "PUSH(FPARG(R11+2));\n" 
	       "CALL(MAKE_SOB_PAIR);\n" 
	       "DROP(2);\n" 
           "DECR(R11);\n" 
           "JUMP(" first-loop-label ");\n" 
	       first-loop-label-end ":\n" 	
	       ;stack fixing 
	       "DROP(4);\n" 
           "DROP(FPARG(1));\n" 
	       "PUSH(R0);\n" 
	       second-loop-label ":\n" 		
	       "CMP(R11, IMM(-1));\n" 
           "JUMP_EQ(" second-loop-label-end ");\n" 
	       "PUSH(FPARG(R11+2));\n" 
           "DECR(R11);\n" 
           "JUMP(" second-loop-label ");\n" 
	       second-loop-label-end ":\n" 
	       "PUSH(IMM(" (number->string (+ params-len 1)) "));\n" 
	       "PUSH(FPARG(0));\n" 
	       "PUSH(FPARG(-1));\n" 
	       "PUSH(FPARG(-2));\n" 
	       "MOV(FP, SP);\n" 
	       "JUMP(" opt-body ");\n" 
	      ;no opt- moving up the stack
	       no-opt-var ":\n" 
	       "PUSH(R4); \n" 
	       "PUSH(R5); \n" 
	       "PUSH(R6); \n" 
	       "MOV(R5, IMM(-2)); //source \n" 
	       "MOV(R6, IMM(-3)); //dest  \n" 
	       "MOV(R4, FPARG(1)); \n" 
	       "ADD(R4, IMM(4)); \n" 
	       third-loop-label ":\n" 
	       "CMP(R4, IMM(0)); \n" 
	       "JUMP_EQ(" third-loop-label-end ");\n" 
	       "MOV(FPARG(R6), FPARG(R5)); \n" 
	       "INCR(R5); \n" 
	       "INCR(R6); \n" 
	       "DECR(R4); \n" 
	       "JUMP(" third-loop-label ");\n" 
	       third-loop-label-end ":\n" 
	       "INCR(FP); \n" 
	       "INCR(SP); \n"
	       "MOV(R12,FPARG(1));\n" 
	       "MOV(FPARG(R12+2), SOB_NIL); \n" 
	       "INCR(FPARG(1)); \n" 
	       "POP(R6); \n" 
	       "POP(R5); \n" 
	       "POP(R4); \n" 
       	   ;run the closure
	       opt-body ":\n" 
	       opt-code-gen-body "\n"
	       "POP(FP);\n" 
	       "RETURN;\n" 
	       opt-exit-label ":\n" 
	       "MOV(R0, R1);\n"
	   
	       ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-define  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-define
  (lambda (pe major)

    (with pe (lambda (def name sexpr)
	        (let ((expr (code-gen sexpr major)))

	       (string-append 	     

	       "MOV(R8," (number->string (get-free-var-address (cadr name)) ) ");\n" 
            expr "\n"
	       "MOV(IND(R8),R0);\n" 
	       "MOV(R0, SOB_VOID);\n" 
	       ))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  or labels       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-or-exit (^^label "L_OR_EXIT_"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-or  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-or
  (lambda (pe major)
    (with pe
	  (lambda (tag seq)
	    (letrec ((or_exit (^label-or-exit))
		         (or-code (map (lambda (exp) (code-gen exp major)) seq))
		         (find-first-true (lambda (x)
			                       (if (null? x) (string-append or_exit ":")
				                      (string-append 
				                      (car x) "\n"
				                      "CMP(R0, SOB_FALSE);\n" 
				                      "JUMP_NE(" or_exit ");\n"
				                      (find-first-true (cdr x)))))))
	                       (find-first-true or-code))
))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-seq  ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define code-gen-seq
  (lambda (pe major)
    (with pe
	  (lambda (tag seq)
	  	(fold-right
	     string-append 
			      		""
	       (map (lambda (x) (code-gen x major)) seq))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-applic  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-gen-applic
  (lambda (pe major)
    (with pe (lambda (applic operator operands)
			    (letrec 
			    	((op     (code-gen operator major))
				     (n      (length operands))
				     (params (reverse (map (lambda (x) (code-gen x major)) operands))))
			      (string-append
			      	(fold-right
			      		string-append 
			      		""
			      		(map (lambda (x)
			      			(string-append
			      				;"MOV(R0, " x");\n"
			      				x"\n"
			      				"PUSH(R0);\n")) params))
			        
			       "PUSH(IMM(" (number->string n) "));\n";
		               op"\n"
			       "CMP(IND(R0), IMM(T_CLOSURE));\n"
			       "JUMP_NE(APPLIC_NOT_A_CLOUSER_ERROR);\n"

		           "PUSH(INDD(R0, 1));\n"
			       "CALLA(INDD(R0, 2));\n"
			       "DROP(1);\n"
			       "POP(R1);\n"
			       "DROP(R1);\n"  ))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  tc-applic labels       ;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-tc-applic-loop     (^^label "TC_APPLIC_LOOP_"))
(define ^label-tc-applic-loop-end (^^label "TC_APPLIC_END_OF_LOOP_"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-tc-applic  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-tc-applic 
  (lambda (pe major)
    (with pe (lambda (tc-applic operator operands)
               (letrec ((tc-applic-loop (^label-tc-applic-loop))
                        (tc-applic-loop-end (^label-tc-applic-loop-end))
                        (op (code-gen operator major))
                        (m (length operands))
                        (params (reverse (our-map (lambda (x) (code-gen x major)) operands))))
                 (string-append 
                  (string-append
                   ;push arguments to stack
                   (fold-right
                    string-append 
                    ""
                    (our-map (lambda (x)
                               (string-append
                                x"\n"
                                "PUSH(R0);\n")) params))
                   ;end of pushing args
                   

                   "PUSH(IMM(" (number->string m) "));\n"   ;push m (number of arguments for )
                   op"\n"
                   "CMP(INDD(R0,0), IMM(T_CLOSURE));\n"
                   "JUMP_NE(APPLIC_NOT_A_CLOUSER_ERROR);\n"  
                   "PUSH(INDD(R0, 1));\n" 		;push env of proc-BLA
                   "PUSH(FPARG(-1));\n"   		;push old return address	

                   "MOV(R1, FPARG(-2));\n"		;copy the old FP  . R4<-old-frame-pointer 
                   "MOV(R5, STARG(1));\n"		;R5 contains m  (number of argument  )
                   "ADD(R5, 3);\n" 
                   "MOV(R3, FPARG(1));\n"  ;R3<- n  number of arguments in stack from old frame.
                   "MOV(R6, IMM(0));\n" 
                   "MOV(SP, FP);\n" ;
                   "SUB(SP,R3);\n"
                   "SUB(SP, 4);\n" ;fp-m-4                 
                   ;loop
                   tc-applic-loop ":\n"  
                   "CMP(R6, R5);\n" 
                   "JUMP_EQ(" tc-applic-loop-end ");\n" 
                   "PUSH(LOCAL(R6));\n" 
                   "INCR(R6);\n" 
                   "JUMP(" tc-applic-loop ");\n" 
                   tc-applic-loop-end ":\n" 
                   "MOV(FP, R1);\n" 
                   "JUMPA(INDD(R0, 2));\n"
                   )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  code-gen-set  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(define code-gen-set
	(lambda (pe major)
		(with pe
			(lambda (set var value)
				(cond ((equal? 'pvar (car var)) (code-gen-set-pvar pe major))
					  ((equal? 'bvar (car var)) (code-gen-set-bvar pe major))
					  ((equal? 'fvar (car var)) (code-gen-set-fvar pe major))
                       (else (display "error in code-gen-set")))))))


(define code-gen-set-pvar
  	(lambda (pe major)
	  (with pe
	    (lambda (set var value)
	(let ((minor       (caddr var))
	      (coded-value (code-gen value major)))
		(string-append

		coded-value "\n"

		"MOV(FPARG(" (number->string (+ minor 2)) "),R0);\n"
		"MOV(R0,IMM(SOB_VOID));\n"
	                 ))))))



(define code-gen-set-bvar
  	(lambda (pe major)
	  (with pe
	    (lambda (set var value)
	(let ((var_major       (caddr var))
		  (var_minor       (cadddr var))
	      (coded-value (code-gen value major)))
		(string-append

		coded-value "\n"
         "MOV(R1, FPARG(0));\n"
         "MOV(R1, INDD(R1, " (number->string var_major) "));\n"
	     "MOV(R1, INDD(R1, " (number->string var_minor)  "));\n"
	     "MOV(R1, R0);\n"
	     "MOV(R0,IMM(SOB_VOID));\n"
	                 ))))))


	
(define code-gen-set-fvar
  	(lambda (pe major)
	  (with pe
	    (lambda (set var value)
	(let ((free_var (number->string (get-free-var-address (cadr var))))
	      (coded-value (code-gen value major)))
		(string-append
		coded-value "\n"
		"MOV(IND("  free_var "),R0);\n"
		"MOV(R0,IMM(SOB_VOID));\n"
	                 ))))))
  
        
;;;;;;;;;;;;;;;;;;;;;;;
;;;  code-gen-box  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;				 
					 
(define code-gen-box
    (lambda (pe major)
	   (with pe
	       (lambda (box var)
		   (let ((coded-var (code-gen var major))
		   		 (minor       (caddr var)))
             (string-append 

		    coded-var "\n"
           "MOV(R1,R0);\n"
           "PUSH(1);\n"
           "CALL(MALLOC);\n"
           "DROP(1);\n"
		  

           "MOV(IND(R0),R1);\n"
           		  

	 	   "MOV(FPARG(" (number->string (+ minor 2)) "),R0);\n"
		  

		   ))))))
					 
				   
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  code-gen-box-get  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;					 
					 

(define code-gen-box-get
	(lambda (pe major)  		
		(with pe
			(lambda (box-get var)
				(cond ((equal? 'pvar (car var)) (code-gen-box-get-pvar pe major))
					  ((equal? 'bvar (car var))(code-gen-box-get-bvar pe major))
                       (else (display "error in code-gen-box-get"))
                   )))))


(define code-gen-box-get-pvar
  	(lambda (pe major)
 	

	  (with pe
	    (lambda (box-get var)
	    (let ((coded-var (code-gen var major))
	          (minor     (caddr var)))
		(string-append
		coded-var "\n"
		"MOV(R0, FPARG(" (number->string (+ minor 2)) "));\n"
		"MOV(R0,IND(R0));\n"
	                 ))))))

(define code-gen-box-get-bvar
  	(lambda (pe major)
	  (with pe
	    (lambda (box-get var)
	    (let ((coded-var (code-gen var major))
	          (var_major     (caddr var))
	      	  (var_minor     (cadddr var)))
		(string-append

		"MOV(R1, FPARG(0));\n"
		"MOV(R1, INDD(R1 ," (number->string var_major) "));\n"
		"MOV(R1, INDD(R1 ," (number->string var_minor) "));\n"
				"MOV(R0,IND(R1));\n"

	                 ))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  code-gen-box-set  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-gen-box-set
	(lambda (pe major)
		(with pe
			(lambda (box-set var value)
				(cond ((equal? 'pvar (car var)) (code-gen-box-set-pvar pe major))
					  ((equal? 'bvar (car var)) (code-gen-box-set-bvar pe major))
                       (else (display "error in code-gen-box-get"))
                   )))))

(define code-gen-box-set-pvar
	(lambda (pe major)
	  (with pe
    (lambda (set var value)
	(let ((minor       (caddr var))
	      (coded-value (code-gen value major)))
		(string-append

		coded-value "\n"
		"MOV(R1 ,FPARG(" (number->string (+ minor 2)) "));\n"
		"MOV(IND(R1),R0);\n"
		"MOV(R0,IMM(SOB_VOID));\n"
	                 ))))))




(define code-gen-box-set-bvar
  	(lambda (pe major)
	  (with pe
	    (lambda (set var value)
	(let ((var_major       (caddr var))
		  (var_minor       (cadddr var))
	      (coded-value (code-gen value major)))
		(string-append
		 coded-value "\n"
         "MOV(R1, FPARG(0));\n"
         "MOV(R1, INDD(R1, " (number->string var_major) "));\n"
	     "MOV(R1, INDD(R1, " (number->string var_minor)  "));\n"
	     "MOV(IND(R1), R0);\n"
	     "MOV(R0,IMM(SOB_VOID));\n"
	                 ))))))



(define code-gen-GUL ;print result of sexpr
	(lambda ()
		(letrec ((next_expr (^label-code-gen-GUL-next) ))
			(string-append
				"\nCMP(R0,SOB_VOID);\n"
				"JUMP_EQ ("next_expr ");\n"
				"PUSH(R0);\n"
				"CALL(WRITE_SOB);\n"
				"DROP(1);\n"
				"CALL(NEWLINE);\n"
				"DROP(1);\n"
				next_expr ":\n"

	))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  lambda-variadic labels     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-variadic-exit              (^^label "L_VARIADIC_CLOSURE_EXIT_"))
(define ^label-variadic-code              (^^label "L_VARIADIC_CLOSURE_CODE_"))
(define ^label-variadic-env-loop          (^^label "L_VARIADIC_CLOSURE_ENV_LOOP_"))
(define ^label-variadic-env-loop-end      (^^label "L_VARIADIC_CLOSURE_ENV_LOOP_END_"))
(define ^label-variadic-param-loop        (^^label "L_VARIADIC_CLOSURE_PARAM_LOOP_"))
(define ^label-variadic-param-loop-end    (^^label "L_VARIADIC_CLOSURE_PARAM_LOOP_END_"))
(define ^label-loop-variadic-code         (^^label "L_LOOP_VARIADIC_CLOSURE_CODE_"))
(define ^label-loop-end-variadic-code     (^^label "L_END_LOOP_VARIADIC_CLOSURE_CODE_"))
(define ^label-no-args-variadic-code      (^^label "L_NO_ARGS_VARIADIC_CLOSURE_CODE_"))
(define ^label-variadic-body-code         (^^label "L_VARIADIC_BODY_CLOSURE_CODE_"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  code-gen-lambda-var  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define code-gen-lambda-var
  (lambda (pe major)
    (with pe
	  (lambda (lambda-var argl body)
	    (letrec 
            ((variadic-code-gen-body    (code-gen body major))
		     (variadic-clos-label       (^label-variadic-code))
		     (variadic-env-loop         (^label-variadic-env-loop))
		     (variadic-env-loop-end     (^label-variadic-env-loop-end))
		     (variadic-param-loop       (^label-variadic-param-loop))
		     (variadic-param-loop-end   (^label-variadic-param-loop-end))
             (variadic-loop-label       (^label-loop-variadic-code))
		     (variadic-loop-label-end   (^label-loop-end-variadic-code))
		     (variadic-no-args          (^label-no-args-variadic-code))
		     (variadic-body             (^label-variadic-body-code))
		     (variadic-exit-label       (^label-variadic-exit)))
	         (string-append
           ;closure memory
           "MOV(R1,FPARG(0))\n" ;old env
            ;creating new env
	       "PUSH(IMM(" (number->string major) "));\n" 
	       "CALL(MALLOC);\n" ;after malloc R0 pointer to block
	       "DROP(1);\n"
	       "MOV(R2, R0);\n" ;R2 <- pointer to new env
	      ;copy env 
	       "MOV(R4, IMM(1));\n" ;j=1
	       "MOV(R5, IMM(0));\n" ;i=0
	       variadic-env-loop ":\n" 
	       "CMP(R4, IMM(" (number->string major) "));\n" 
	       "JUMP_GE(" variadic-env-loop-end ");\n" ;jump if i>= major to end of loop
	       ;loop body
	      ; "MOV(INDD(R2, R4), INDD(ENV, R5));\n"
	       "MOV(INDD(R2, R4), INDD(R1, R5));\n" ;R2[j] = R1[i]
           "INCR(R4);\n"  ;j++
	       "INCR(R5);\n"  ;i++
	       "JUMP(" variadic-env-loop ");\n" 
	        variadic-env-loop-end ":\n" 
            ;creating memory for args. parameters for new env
           "MOV(R3, FPARG(1));\n" ; R3<- n (number of argument)
	       "PUSH(R3);\n"
	       "CALL(MALLOC);\n" 
	       "DROP(1);\n"
	       "MOV(IND(R2), R0);\n" 
	       "MOV(R10,IND(R2));\n"
	       ;IND(R2) = NEW ENV
            ;copy args
	       "MOV(R4, IMM(0));\n" ;i=0
	       ;"MOV(R5, IMM(2));\n" ;j=2 ;OR add
	        variadic-param-loop":\n"
	       "CMP(R4, R3);\n"
	       "JUMP_EQ(" variadic-param-loop-end ");\n"
	       "MOV(INDD(R10,R4), FPARG(R4+2));\n"
	       "INCR(R4);\n" 
	       "JUMP(" variadic-param-loop ");\n"
	       variadic-param-loop-end ":\n" 
             ;memory for closure
           "PUSH(IMM(3));\n"
	       "CALL(MALLOC);\n"
	       "DROP(1);\n"              
	       "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"  
	       "MOV(INDD(R0, 1), R2);\n"
	       "MOV(INDD(R0, 2), LABEL(" variadic-clos-label "));\n" 
	       "MOV(R1,R0);\n"
	       "JUMP(" variadic-exit-label ");\n"
            ;clousre code
	       variadic-clos-label  ":\n"
	       "PUSH(FP);\n"
	       "MOV(FP, SP);\n"
		 	       
            ;reorgenize stack
	       "PUSH(R4);\n" 
	       "MOV(R4, FPARG(1));\n" 
	       "CMP(R4, 0);\n"
	       "JUMP_EQ(" variadic-no-args");\n"
           "MOV(R0, SOB_NIL);\n" 
            "DECR(R4);\n" 
	       variadic-loop-label ":\n" 
	       "CMP(R4, -1);\n" 
           "JUMP_EQ(" variadic-loop-label-end ");\n" 
	       "PUSH(R0);\n" 
	       "PUSH(FPARG(R4+2));\n" 
	       "CALL(MAKE_SOB_PAIR);\n"
	       "DROP(2);\n" 
           "DECR(R4);\n" 
           "JUMP(" variadic-loop-label ");\n"
	       variadic-loop-label-end ":\n" 
	       "POP(R4);\n" 
	       ;stack fixing
	       "DROP(4);\n" 
           "DROP(FPARG(1));\n"
	       "PUSH(R0);\n" 
	       "PUSH(IMM(1));\n" 
	       "PUSH(FPARG(0));\n" 
	       "PUSH(FPARG(-1));\n" 
	       "PUSH(FPARG(-2));\n" 
	       "MOV(FP, SP);\n" 
	       "JUMP(" variadic-body ");\n" 
	       variadic-no-args ":\n" 
	       "POP(R4);\n" 
	       "MOV(FPARG(-3), FPARG(-2));\n" 
	       "MOV(FPARG(-2), FPARG(-1));\n" 
	       "MOV(FPARG(-1), FPARG(0));\n" 
	       "MOV(FPARG(0), IMM(1));\n" 
	       "MOV(FPARG(1), SOB_NIL);\n" 
	       "INCR(FP);\n" 
	       "INCR(SP);\n" 
	       variadic-body ":\n" 
	       variadic-code-gen-body "\n"
	       "POP(FP);\n" 
	       "RETURN;\n" 
	       variadic-exit-label ":\n" 
	       "MOV(R0, R1);\n" 
	       ))))))

