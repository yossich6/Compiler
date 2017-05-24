 (load "pattern-matcher.scm")
  (load "pc.scm")


(define <digit-0-9>
  (range #\0 #\9))


(define <digit-1-9>
  (range #\1 #\9))

(define <natural> 
  (new 
     (*parser (char #\0)) *plus
     (*parser <digit-1-9>)
     (*parser  <digit-0-9>) *star
     (*caten 3)
     (*pack-with 
        (lambda (z x xs)
          (string->number (list->string `(,x ,@xs)))))


     (*parser <digit-1-9>)
     (*parser  <digit-0-9>) *star
     (*caten 2)
     (*pack-with 
        (lambda (x xs)
          (string->number (list->string `(,x ,@xs)))))


     (*parser (char #\0)) *plus
       (*pack (lambda (_) 0))

     (*disj 3)

    done))

(define <Integer>
  (new (*parser (char #\+))
     (*parser <natural>)
     (*caten 2)
     (*pack-with 
        (lambda (++ num)
          (+ num ) ))

     (*parser (char #\-))
     (*parser <natural>)
     (*caten 2)
     (*pack-with
      (lambda (-- n) (- n)))

     (*parser <natural>)

     (*disj 3)
    done))

(define <Fraction>
  (new (*parser <Integer> )
     (*parser (char #\/))
     (*parser <natural> )
     (*guard (lambda (n) (not (zero? n))))
     (*caten 3)
     (*pack-with 
      (lambda (int div num)
        (/ int num)))
    done))
; add fractions


(define <Number>
  (new  
     (*parser <Fraction>)
      (*parser <Integer>)
     (*disj 2)
    done))

(define <Boolean>
  (new
    (*parser (word-ci "#t"))
    (*pack
    (lambda (n) #t) )

    (*parser (word-ci "#f"))
    (*pack
    (lambda (n) #f) )
    (*disj 2)
  done))




;//// gets "\\\\" and #\\  will pring #\\

(define ^<meta-char> 
  (lambda (str ch)
  (new (*parser (word-ci str))
     (*pack (lambda (_) ch))
     done)))



;/////


(define <a-f>
  (range #\a #\f))

(define <A-F>
  (range #\A #\F))

(define <HexChar>
     (new
      (*parser <digit-0-9>) 
      (*parser <a-f>)
      (*parser <A-F>)
      (*disj 3)
     done))


;<HexUnicodeChar> ::= x<hexChar>+
(define <HexUnicodeChar>
  (new
    (*parser (char-ci #\x))
    (*parser <HexChar>) *plus
    (*caten 2)
    (*pack-with 
      (lambda (x ch) (integer->char (string->number (list->string ch) 16))) )
    ;(lambda (x ch) (string->number (list->string ch) 16))) 
    ;(*guard (lambda (num)  (< num  1114111) ))

    ;(lambda (ch) (integer->char ch ))
  done))

;/////
(define <whitespace>
  (const 
    (lambda(ch)
      (char<=? ch #\space))))


(define <VisibleSimpleChar> 
  (range #\x21 #\xff) )

(define <CharPrefix>
  (new
    (*parser (word "#\\" )) 
    done))

(define <NamedChar>
 (new 

    (*parser (^<meta-char> "lambda"  (integer->char 955)))
    (*parser (^<meta-char> "newline" (integer->char 10) ))
    (*parser (^<meta-char> "nul" (integer->char 0)))
    (*parser (^<meta-char> "page" (integer->char 12)))  
    (*parser (^<meta-char> "return" (integer->char 13)))
    (*parser (^<meta-char> "space" (integer->char 32) ))
    (*parser (^<meta-char> "tab" (integer->char 9) ))

    (*disj 7 )
    done))

(define <Char>
  (new 
    (*parser <CharPrefix> )
    
    (*parser <NamedChar> )
    (*parser <HexUnicodeChar> )
    (*parser <VisibleSimpleChar> )

    (*disj 3)

    (*caten 2)
    (*pack-with 
      (lambda (prefix ch) ch ))

  done))


;//////////////////////////////////////////////////////
;             Strings
;//////////////////////////////////////////////////////

(define <StringLiteralChar> 
  (new 
    (*parser <any-char>)
    (*parser (char #\\))
    *diff

  done))


(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f"  (integer->char 12)))

       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" (integer->char 13)))

    (*disj 6)
    

  done))


(define <StringHexChar>
  (new (*parser (char #\\))
     (*parser (char-ci #\x))
     (*parser <HexChar> ) *star
     (*parser (char #\;))

     (*caten 4)
     (*pack-with 
        ;(lambda (slash x ch end) (integer->char (string->number (list->string ch) 16))) )
      (lambda (slash x ch end)  (string->number (list->string ch) 16)) )
     
     (*guard (lambda (num)  (< num  1114111) ))
     (*pack (lambda (x) (integer->char x) ))

  done))

(define <StringChar>
  (new (*parser <StringHexChar>)
    (*parser <StringMetaChar>)
    (*parser <StringLiteralChar>)
    (*disj 3)
    (*parser (char #\"))
    *diff
  done))

(define <String> 
  (new (*parser (char #\"))
     (*parser <StringChar>) *star
     (*parser (char #\"))
     (*caten 3)

  (*pack-with
        (lambda (open-delim chars close-delim)
            (list->string chars)))

  done))
;//////////////////////////////////////////////////////
;             symbol
;//////////////////////////////////////////////////////


(define <SymbolChar>
  (new (*parser  (range #\0 #\9))
     (*parser  (range #\a #\z))
     (*parser  (range #\A #\Z))
     (*pack (lambda (ch) (char-downcase ch)))
     (*parser   (char #\!))
     (*parser   (char #\$))
     (*parser   (char #\^))
     (*parser   (char #\*))
     (*parser   (char #\-))
     (*parser   (char #\_))
     (*parser   (char #\=))
     (*parser   (char #\+))
     (*parser   (char #\<))
     (*parser   (char #\>))
     (*parser   (char #\?))
     (*parser   (char #\/))
     (*parser   (char #\:))

  (*disj 16)
  done))


(define <Symbol> 
  (new (*parser <SymbolChar>)*plus
   (*pack 
   (lambda (ch) 
    (string->symbol (list->string ch))))
  done))




;//////////////////////////////////////////////////////
;             ProperList
;//////////////////////////////////////////////////////
(define <space>
  (new (*parser (char #\ ))  *star
    done))



(define <ProperList111>
  (new  ;(*parser (char #\( ) )  
      
      
        (*parser (^<separated-exprs> (*delayed (lambda () <sexpr> ) ) <space> ) )

        
        ;(*parser (char #\) ) ) 
       ;(*caten 3)
     ;  (*pack-with
     ;    (lambda (openBar expr closeBar) expr ))
  done))



 ;***************** Mayer part  Start ******************

(define <line-comment>
  (let ((<end-of-line-comment>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))
   
   (*parser <any-char>)
   (*parser <end-of-line-comment>)
   *diff *star

   (*parser <end-of-line-comment>)
   (*caten 3)
   done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>)) ;;;;;;
       (*delayed (lambda () <sexpr>))
       (*disj 2)  ;;;;;
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
  <sexpr-comment>))

 (define <skip>
  (disj <comment>
    <whitespace>))

 (define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
     (*parser <p>)
     (*parser <wrapper>)
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done))))


(define ^<skipped*> (^^<wrapped> (star <skip>)))

1
 ;***************** Mayer part  END ******************

 ;////////////////////////////

(define <spaceRemove> 
  (new (*parser <whitespace>) *star
     (*delayed (lambda () <sexpr>))
     (*parser <whitespace>) *star
     (*caten 3)
     (*pack-with (lambda (s1 exps s2) exp) )

  done))

;//////////////////////////////////////////////////////
;             sexpr
;//////////////////////////////////////////////////////
(define <sexpr> 
  (^<skipped*>
    (new
      (*parser  <Boolean>)
      (*parser  <Char>)

      (*parser  <Number>) 

      (*parser  <Symbol>)
      (*parser <digit-0-9>)
      *diff
        *not-followed-by

      (*parser  <Symbol>)

      (*parser  <String>)
      (*delayed (lambda () <ProperList>))
      (*delayed (lambda () <ImproperList>))
      (*delayed (lambda () <Vector>))
      (*delayed (lambda () <Quoted>))
      (*delayed (lambda () <QuasiQuoted>))
      (*delayed (lambda () <Unquoted>))
      (*delayed (lambda () <UnquoteAndSpliced>))
      (*delayed (lambda () <InfixExtension>))
      ;(*parser <spaceRemove>)

      
    ; (*parser  <InfixExtension>)

      (*disj 13)  

  done)));)



;;;
; (test-string <sexpr> "(123 \"\\x6a;\" #t 1/4)" )
(define <CloseBarcket>
  (new (*parser <whitespace>) *star
      (*parser (char #\) ) )
     (*parser <whitespace>) *star

      (*caten 3)
       ; (*pack-with
          ;(lambda (openBar expr) expr ))
  
  done))
 (define <OpenBarcket>
  (new (*parser <whitespace>) *star
      (*parser (char #\( ) ) 
        (*parser <whitespace>) *star

      (*caten 3)
       ; (*pack-with
          ;(lambda (openBar expr) expr ))
  
  done))


(define <ProperList>
  (new   
      (*parser <OpenBarcket>)
      
  
      
      (*parser (^<separated-exprs> <sexpr>   <space>  ) )
      

        (*parser <CloseBarcket>)
       (*caten 3)
      (*pack-with
        (lambda (openBar expr closeBar) expr ))


      (*parser (char #\( ));empty no args
      (*parser (char #\)) )
      (*caten 2)
      (*pack-with
      (lambda (op cl)
                `() ))
      (*disj 2)

  done))


 (define <ImproperList>
  (new ;(*parser (char #\( ) )  
      (*parser <OpenBarcket>)
          (*parser <sexpr>) *plus
      (*parser (char #\.))
      (*parser <sexpr>)
      ;(*parser (char #\) ) )  
          (*parser <CloseBarcket>)

      (*caten 5)
      (*pack-with
      (lambda (a exp1 dot exp2 e)
          `(,@exp1 . ,exp2)
                            ))
    done))


  (define <Vector>
    (new (*parser (char #\#))
       (*parser <ProperList>)
        (*caten 2)
        (*pack-with
      (lambda (ch lst)
          (list->vector lst)  ))

    done))

 (define <Quoted>
    (new (*parser (char #\'))
       (*parser <sexpr>) 
        (*caten 2)
       (*pack-with
      (lambda (quo expr)
          (list 'quote expr)))

    done))

 (define <QuasiQuoted>
    (new (*parser (char #\`))
       (*parser <sexpr>) 
        (*caten 2)
       (*pack-with
      (lambda (quasi expr)
          (list 'quasiquote expr)))

    done))

 (define <Unquoted>
    (new (*parser (char #\,))
       (*parser <sexpr>) 
        (*caten 2)
       (*pack-with
      (lambda (quo expr)
          (list 'unquote expr)))

    done))

 (define <UnquoteAndSpliced>
    (new (*parser (char #\,))
       (*parser (char #\@))
       (*parser <sexpr>) 
        (*caten 3)
       (*pack-with
      (lambda (unquo s expr)
          (list 'unquote-splicing expr)))

    done))

;----------------------------------------------------------------------------

(define <InfixPrefixExtensionPrefix>
  (new (*parser (char #\#))
     (*parser (char #\#))
     (*caten 2)

     (*parser (char #\#))
     (*parser (char #\%))
     (*caten 2)

     (*disj 2)
  done))


(define <Operators>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*parser (char #\*))
       (*parser (word "**"))
       (*parser (char #\^))
       (*parser (char #\/))
       (*disj 6)
  done))


(define <InfixSymbol>
  (new  (*parser <SymbolChar>)
      (*parser <Operators> )
    *diff
     *plus

     (*guard 
      (lambda (infixNumberSymbol) 
      (<Number> infixNumberSymbol
         (lambda (n remaining-chars )
          (if (null? remaining-chars) #f #t))
         (lambda (_) #t))))

     
    (*pack (lambda (sym)
     (string->symbol (list->string sym))))
  done))

(define <PlusOrMinus>  
  (new (*delayed (lambda () <MulOrDiv>))
 
        (*parser (^<skipped*> (char #\+ ) ) )
      (*parser (^<skipped*> (char #\- ) ) )
      (*disj 2)
      (*delayed (lambda () <MulOrDiv>))
          (*caten 2)

    (*pack-with 
        (lambda (oprator exp)
          (lambda (elment) 
            (list (string->symbol (string oprator)) elment exp) )))
      
      *star
      (*caten 2)

      (*pack-with 
        (lambda (e1 lmbe2)
          (fold-left 
            (lambda (elm lamb)  
              (lamb elm))
            e1 lmbe2)))
    done))
            

(define <MulOrDiv> 
    (new (*delayed (lambda () <Power>))

          (*parser (^<skipped*> (char #\* ) ) )
      (*parser (^<skipped*> (char #\/ ) ) )
      (*disj 2)
      (*delayed (lambda () <Power>))
      (*caten 2)
      (*pack-with
       (lambda (oprator exp)
          (lambda (elment) 
            (list (string->symbol (string oprator)) elment exp) )))

      *star
      (*caten 2)

      (*pack-with 
        (lambda (e1 lmblis)
          (fold-left 
            (lambda (elm lamb)  
              (lamb elm))
            e1 lmblis)))          
  done))


(define <Power> 
  (new (*delayed (lambda () <Helper> ))

          (*parser (^<skipped*> (char #\^ ) ) )
      (*parser (^<skipped*> (word "**" ) ) )
      (*disj 2)
      (*caten 2)
      

      (*pack-with 
        (lambda (exp oprator)
          (lambda (elment) 
            (list 'expt exp elment) )))
      *star

      (*delayed (lambda () <Helper> ))

      (*caten 2)
      (*pack-with 
       (lambda (lmblis e1)
          (fold-left
            (lambda (elm lamb)  
              (lamb elm))
              e1 (reverse lmblis))))    
  done))

(define <InfixNeg>
  (new (*parser  (char #\-))
      (*delayed (lambda () <Helper>))
      (*caten 2)
      (*pack-with
      (lambda (minus exp)
            ;  (- exp)))
                 `(,(string->symbol (string minus)),exp)))
      done))


(define <Helper> 
  (new (*parser <skip>) *star
     (*delayed (lambda () <InfixFuncall>))
     (*delayed (lambda () <InfixArrayGet>))
    ;     (*parser <InfixNeg>)

     (*delayed (lambda () <numExp>))
     (*delayed (lambda () <InfixSexprEscape> ))
     (*disj 4)
    (*parser <skip>) *star
    (*caten 3)
    (*pack-with
    (lambda (sk1 exp sk2) exp ))

     
  done))


(define <numExp> 
  (new (*parser <InfixSymbol> )
      (*parser <Number> )
      (*parser <InfixNeg>)
      
     ; (*parser <InfixSymbol> )
      
      (*parser <OpenBarcket> )  ;'('
          (*parser <PlusOrMinus> )
      (*parser <CloseBarcket>)  ;')'
      (*caten 3)
      (*pack-with 
        (lambda (openBar exp closeBar) exp ))

      (*disj 4)

  done))




(define <InfixArrayGet>
  (new (*parser <numExp>)

    
    (*parser  (^<skipped*> (char #\[ ) ))
    (*delayed (lambda () <InfixExpression>))
    (*parser (^<skipped*> (char #\])) )
      (*caten 3)
      (*pack-with
      (lambda (o infExp c)
        (lambda (elment)
          `(vector-ref ,elment ,infExp ))))
      *plus
      
      (*caten 2)
      (*pack-with 
        (lambda (numE vec)
          (fold-left (lambda (elem func)(func elem ))
            numE vec)))
      
     done))

;(f (x, y,g(x,z)) -> (f x y z) )

(define <InfixArgList>
  (new 
      (*parser <OpenBarcket>)
      
          (*delayed (lambda () <InfixExpression>))
      
      (*parser (char #\, ))
          (*delayed (lambda () <InfixExpression>))
      (*caten 2)
      (*pack-with (lambda (comma infixExp) infixExp))
      *star
      
      (*parser <CloseBarcket>)
      (*caten 4) 
      (*pack-with
          (lambda (open  infExp lstInfixExp close)
              (lambda (function)
                `(,function ,infExp  ,@lstInfixExp))))
    
      (*parser <OpenBarcket>);empty no args
      (*parser <CloseBarcket>)
      (*caten 2)
      (*pack-with
      (lambda (op cl)
              (lambda (function)
                `(,function ))))
      (*disj 2)

  done))

(define <InfixFuncall>
  (new (*parser <numExp>)
    (*parser <InfixArgList>)  *plus
      (*caten 2)
      (*pack-with 
        (lambda (func args) 
          (fold-left (lambda (elem lam)
                (lam elem) ) func   args )))
  done))




;-----------------------

(define <InfixExpression>
  (^<skipped*>
   (disj <PlusOrMinus> )))


(define <InfixExtension> 
  (new (*parser <InfixPrefixExtensionPrefix>)
     (*parser <InfixExpression>)
     (*caten 2)
     (*pack-with (lambda (inpre expr) expr ))
  done))

(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix> )
     (*parser <sexpr>)
     (*caten 2)
     (*pack-with
     (lambda (pre exp) exp))
    done))

(define <Sexpr> <sexpr>)













(define *void-object* 
  (void) )

(define beginify
  (lambda (s)
    (cond
      ((null? s) *void-object*)
      ((null? (cdr s)) (cdr s))
      (else `(begin ,@s)))))

(define simple-const? 
  (lambda (x) 
    (or (null? x) (vector? x) (boolean? x) (char? x) (number? x) (string? x))))

(define *reserved-words*
  '(and begin cond define do else if lambda let let* 
        letrec or quasiquote unquote unquote-splicing quote set!))


(define var? 
  (lambda (v) 
    (and (symbol? v) 
         (not (member v *reserved-words* ))))) 


	
;check a list to see if there are duplicates .
;Return true if there are no duplicates
(define hasNoDups?
  (lambda (lst)
    (if  (null? lst) 
         #t 
         (if (member (car lst) (cdr lst))
             #f ;there is duplicate we failed. 
             (hasNoDups? (cdr lst)) )
         )))


(define checkLambdaOptArg?
  (lambda (p)
    (if(pair? p)
       (and (var? (car p)) (checkLambdaOptArg? (cdr p) ))
       (var? p))
    ))


(define eraseNestedSeq
  (lambda (exp)
    (letrec ((nestedSeq (lambda (nestExp)
                          (if (equal? (car nestExp) 'seq)  
                              (fold-left append '() (map nestedSeq (cadr nestExp))) 
                              (list nestExp) ))))
      (list 'seq (fold-left append '() (map nestedSeq (cadr exp)))))))






(define beginCare
  (lambda (expr) 
    (letrec  
        ((beg (lambda (expr) 
                (if (null? expr) 
                    (list) 
                    (cons (parse (car expr)) (beg (cdr expr))))))) 
      `(seq ,(beg expr)) 
      ))
  )

(define let-var?
  (lambda (letvar)
    (and (list? letvar)
         (andmap list? letvar)
         (andmap pair? letvar)
         ;check that the letvar has a variable .
         (andmap (lambda (item) (var? (car item))) letvar) 	
         
         )))



(define get-args 
  (lambda (arg)
    (if (pair? arg)
        (cons (car arg) (get-args (cdr arg)))
        '()) 
    ))
(define get-rest
  (lambda (rest)
    (if (pair? rest) (get-rest (cdr rest)) rest)))


;;***********************************************************
;;				Mayer QQ
;;***********************************************************

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;					PARSE 			              	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse
  (let ((run
         (compose-patterns
          (pattern-rule
           (? 'c simple-const?)
           (lambda (c) `(const ,c)))
          
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c) `(const ,c)))
          
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;        IF          ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;    
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit)
             `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
          
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif)
             `(if3 ,(parse test) ,(parse dit) ,(parse dif))))      
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		OR  		        ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	  
          (pattern-rule
           `(or ,(? 'first) . ,(? 'rest))
           (lambda (first rest)
             (if (null? rest) 
                 `( ,@(parse first) )
                 `(or (,(parse first) ,@(map parse rest) )))))
          
          (pattern-rule 
           `(or )
           (lambda () 
             (parse #f)))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		Simple-lambda   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(lambda ,(? 'parameter list? hasNoDups? (lambda (v) (andmap var? v)) ) ,(? 'body) . ,(? 'rest ) )
           (lambda (para body rest)
             (let ((newbody (cons body rest)))
               `(lambda-simple ,para ,(parse `(begin ,@newbody ) ) )))) 
          
          
          
          ;lambda-opt (lambda (x y . rest) exp )
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		Lambda-opt      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(lambda ,(? 'parameter pair?  checkLambdaOptArg?)  ,(? 'body) . ,(? 'rest ) )
           (lambda (para body rest )
             (let ((newbody (cons body rest))
                   (args   (get-args para))
                   (rest   (get-rest para)))
               `(lambda-opt ,args ,rest ,(parse `(begin ,@newbody ) ) )))) 
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		Lambda-var      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(lambda ,(? 'parameter var?) ,(? 'body). ,(? 'rest ) )
           (lambda (para body rest)
             (let ((newbody (cons body rest)))
               
               `(lambda-var ,para ,(parse `(begin ,@newbody)) ))))
          
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	  Regular define  ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(define ,(? 'var var?) .  ,(? 'expr) )
           (lambda (var expr)
             `(def ,(parse var) ,(parse  `(begin ,@expr) ))))
          
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		MIT-style define      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(define ,(? 'var  ) ,(? 'expr) . ,(? 'rest) )
           (lambda (var expr rest)
             `(def ,(parse (car var)) ,(parse `(lambda ,(cdr var) ,expr ,@rest) ))))
          
          ; MIT-style define <-----Need to check if this case is ligell or mayer parse has a bug.
          ;(define a b c d) ==> (def (var a) (seq ((var b) (var c) (var d))))
          (pattern-rule
           ;	`(define ,(? 'var  ) ,(? 'expr) )
           `(define ,(? 'var  ) ,(? 'expr) . ,(? 'rest) )
           (lambda (var expr rest)
             `(def ,(parse (car var)) ,(parse `(lambda ,(cdr var) ,expr ,@rest) ))))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	Applications     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule 
           `( ,(? 'expr (lambda (v) (not (member v *reserved-words* )))) . ,(? 'rest ))
           (lambda (expr rest)
             `(applic ,(parse expr) ,(map parse rest) )))
          
          
          ;;application
          
          (pattern-rule 
           `(begin . ,(? 'expr list? ) )
           (lambda (exprs)
             (cond ( (null? exprs) 	`(const ,*void-object*) ) 
                   ( (equal? (length exprs) 1) (parse (car exprs)))
                   (else 	
                    (eraseNestedSeq (beginCare exprs))
                    ))))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	   	Set! 		      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	  
          (pattern-rule
           `(set! ,(? 'var) ,(? 'expr ) )
           (lambda (var expr)
             `(set ,(parse var) ,(parse expr) ) ) 
           )
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	   	And 	      	;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	 
          (pattern-rule
           `(and .,(? 'expr) )
           (lambda (expr)  ; `(,first ,rest)))
             (if (null? expr) 
                 (parse #t) 
                 (let ((first (car expr))
                       (dit (cdr expr)))
                   
                   (cond ((= (length expr) 1) (parse first )) 
                         (else 
                          (parse `(if  ,first  (and ,@dit) #f))))))))
          
          (pattern-rule
           `(and )
           (lambda () (parse #t)))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		    Cond 		    ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	  
          (pattern-rule
           `(cond ( ,(? 'test) ,(? 'dit ) .,(? 'firstRest ) ) . ,(? 'rest) )
           (lambda (test dit ditRest rest)
             ;`(,test ,dit ,ditRest  ! ,rest)))
             (let ( ( dit   (cons dit ditRest ) ))
               (cond ((and (null? rest) (not (equal? 'else test) ) ) (parse `(if ,test  (begin ,@dit)  )))
                     ((and (null? rest) (equal? 'else test)  ) (parse `(begin ,@dit)) )
                     
                     (else (parse `(if ,test  (begin ,@dit) (cond ,@rest ) ) )))
               )))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;     Quasiquote     ;; 
          ;;;;;;;;;;;;;;;;;;;;;;;;
          (pattern-rule
           `(quasiquote .,(? 'expr-list list?))
           (lambda ( expr-list)
             (parse (expand-qq (car expr-list) ))))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		let 	         	;;
          ;;;;;;;;;;;;;;;;;;;;;;;;
          (pattern-rule
           `(let () ,(? 'body))
           (lambda (body)
             (parse `((lambda () ,body) ))))
          
          (pattern-rule
           `(let ,(? 'varval  let-var?)  ,(? 'body) . ,(? 'rest list?))
           (lambda (var body rest)
             (let ((variables (map car var))
                   (values    (map cadr var) ))
               (parse `((lambda ,variables ,@(cons body rest)) ,@values)))))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		let* 		        ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;
          (pattern-rule
           `(let* () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (let ((b (cons expr exprs) )) 
               (parse `(let ()  ,@b)))))
          
          
          (pattern-rule
           `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
           (lambda (var val rest exprs)	
             (if (not (null? rest))	
                 (parse `(let ((,var ,val))
                             (let* ,rest . ,exprs)))
                 (parse `(let ((,var ,val))
                             ,@exprs)))))
          

          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		letrec*       	;;
          ;;;;;;;;;;;;;;;;;;;;;;;;              
          (pattern-rule
           `(letrec ,(? 'expr list?) ,@(? 'body))
           (lambda (expr body)
             (let ((variables (map car expr)))
               (parse `((lambda ,variables
                            (begin ,@(append (map (lambda (var_val) (list 'set! (car var_val) (cadr var_val))) expr) 
                                             `(((lambda () ,@body))))))
                          ,@(map (lambda (x) #f) expr))))))
          
          
          
          
          
          
          ))) 
    (lambda (e)
      (run e
           (lambda ()
             (error 'parse
                    (format "I can't recognize this: ~s" e)))))))


(define parse-2 parse)









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 3:                                                                               ;;
;;                                                                                             ;;
;;                                                                                             ;;
;;                                                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-it-lambda?
  (lambda (exp)
    (if (and (list? exp)(not(null? exp))(or (eq? (car exp) 'lambda-simple) (eq? (car exp) 'lambda-var) (eq? (car exp) 'lambda-opt))) #t #f)))


(define get-lambda-body
  (lambda (exp)
    (cond  ((eq? (car exp) 'lambda-simple) (with exp (lambda (tagName args body)  body)))
           ((eq? (car exp) 'lambda-var)  (with exp (lambda (tagName arg body )   body )))
           ((eq? (car exp) 'lambda-opt) (with exp (lambda (tagName args optArg body )  body)))
          (else (cadr exp)))))

          
(define get-lambda-parameters
  (lambda (exp)
    (cond ((eq? (car exp) 'lambda-simple) (with exp (lambda (tagName args body) args)))
          ((eq? (car exp) 'lambda-var)  (with exp (lambda (tagName arg body )  (list arg) )))
          ((eq? (car exp) 'lambda-opt) (with exp (lambda (tagName args optArg body ) (append args (list optArg))  )))
          (else (cadr exp)))))
                 
(define set-lambda-body
  (lambda (old-lambda new-body)
    (cond ((eq? (car old-lambda) 'lambda-simple)  `( ,(car old-lambda) ,(get-lambda-parameters old-lambda) ,new-body ))
          ((eq? (car old-lambda) 'lambda-var)  `( ,(car old-lambda) ,@(get-lambda-parameters old-lambda) ,new-body ))
          ((eq? (car old-lambda) 'lambda-opt)  `( ,(car old-lambda) ,(cadr old-lambda) ,(caddr old-lambda) ,new-body ) )
          (else  #f ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  flatenListOfLists  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flatenListOfLists
  (lambda (lst) (fold-right append '() lst)))

 
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Eliminate-nested-defines    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eliminate-nested-defines-begin 
  (lambda (pes ret-ds+es )
  (if (null? pes) (ret-ds+es '() '() )
     (eliminate-nested-defines-begin (cdr pes) 
      (lambda (ds es) 
        (cond ((and (not(null? (car pes))) (list? (car pes)) (eq? (caar pes) 'def )) (ret-ds+es (cons (car pes) ds) es))
            ((and (not(null? (car pes))) (list? (car pes)) (eq? (caar pes) 'seq)) (eliminate-nested-defines-begin (cadar pes) 
                          (lambda (ds1 es1) 
                            (ret-ds+es (append ds1 ds) (append es1 es)))))
                        (else  (ret-ds+es ds (cons (car pes) es )))) 

        )))

))

(define reserved-words-letrec 
  '(if3 set seq lambda-simple lambda-opt lambda-var const var applic and begin cond define do else if lambda let let* 
        letrec or quasiquote unquote unquote-splicing quote set!))

(define var-Guard 
  (lambda (v) 
  (not(and (list? v) (list? (car v) ) (eq? (caar v) 'var ) ) ) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    parse-letrec              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-letrec
  (let ((run
       (compose-patterns

          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;    letrec*         ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;          
    (pattern-rule
           `(letrec ,(? 'expr list?) ,@(? 'body))
           (lambda (expr body)
             (let ((variables (map cadar expr)))
              (parse-letrec  `( applic (lambda-simple ,variables
                          
                            (seq ,(append (map (lambda (var_val) 
                              (list 'set (car var_val)  (parse-letrec (get-lambda-body var_val) ) )) expr) 
                          
                                             `(  ,@body ) )))
                          ,(map (lambda (x) `(const #f)) expr)) ))))

     
     	    ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;  Applications      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;
     (pattern-rule 
           `( ,(? 'expr   (lambda (v) (not (member v reserved-words-letrec   ))) ) . ,(? 'rest ))
           (lambda (expr rest) 
            
             `(  ,(parse-letrec expr) ,@(map parse-letrec rest) )) )
  
     (pattern-rule 
      `(applic ,(? 'expr  (lambda (v) (not (member v reserved-words-letrec   ))) var-Guard ) . ,(? 'rest) )
      (lambda (expr rest) 
         `(applic ,(parse-letrec expr) ,@rest )))

   (pattern-rule 
      `(const ,(? 'const ) )
      (lambda (x)  `(const ,x) ))
     
     (pattern-rule 
      `(var ,(? 'value ) )
      (lambda (x) `(var ,x) ))

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;;        IF          ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;        
    (pattern-rule 
    `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
     (lambda (test dit dif)
      `(if3 ,(parse-letrec test) ,(parse-letrec dit) ,(parse-letrec dif))))      

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;;    OR              ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;    
    (pattern-rule
     `(or ,(? 'first) . ,(? 'rest))
     (lambda (first rest)
         
           `(or ,(parse-letrec first) ,@(map parse-letrec rest) )))   
   ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;      Seq          ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;   
  
     (pattern-rule 
      `(seq ,(? 'exp ) )
      (lambda (x)  `(seq ,(parse-letrec x) )))
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;      Set!          ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;    
      (pattern-rule
          `(set ,(? 'var) ,(? 'expr ) )
           (lambda (var expr)
             `(set ,var ,expr ) ) )
           
       
     (pattern-rule 
      `(lambda-simple ,(? 'var ) ,(? 'body) )
      (lambda (para body )  `(lambda-simple ,para ,(parse-letrec body))))
      
          
     (pattern-rule 
      `(lambda-var ,(? 'var ) ,(? 'body) )
      (lambda (para body )  `(lambda-var ,para ,(parse-letrec body))))
        ;;;;;;;;;;;;;;;;;;;;;;;;
        ;;      lambda-opt!   ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;
         (pattern-rule 
      `(lambda-opt ,(? 'var ) ,(? 'rest) ,(? 'body) )
      (lambda (para rest body ) 
       `(lambda-opt ,para ,rest ,(parse-letrec body))))
       
          
   )))

        (lambda (e)
      (run e
           (lambda ()
             (error 'parse-letrec
                    (format "I can't recognize this: ~s" e)))))      
))



(define nested-var
          (lambda (exp)
            (map (lambda(x) (cadr x)) exp)))
(define nested-value
          (lambda (exp)
            (map (lambda(x) (caddr x)) exp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  make-letrec-map-func        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-letrec-map-func
   (lambda (x lambdaKind para)
    
    (if (and (list? x) (or (eq? (car x) 'lambda-simple) (eq? (car x) 'lambda-opt) (eq? (car x) 'lambda-var))) 
      (make-letrec x (car x) (cadr x))
   (make-letrec x lambdaKind para))))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    make-letrec               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-letrec
  (lambda (exp lambdaKind para)
    (let ((nestedExp (if (is-it-lambda? exp) 
                         (if (equal? (car exp) 'lambda-opt)
                             (eliminate-nested-defines-begin (cddr exp) (lambda (x y) (list x (cdr y)))) 
                             (eliminate-nested-defines-begin (cdr exp) (lambda (x y) (list x (cdr y)))))
                         exp)))
      
                            
    (cond ((not(is-it-lambda? exp)) exp)
        ((null? (car nestedExp)) exp)
    (else 
      (set-lambda-body exp 
       (append (list 'letrec (combineVarVal (nested-var(car nestedExp)) 
     (map (lambda (x)(make-letrec-map-func x lambdaKind para))  (nested-value (car nestedExp)))))
       (map (lambda (x)(make-letrec-map-func x lambdaKind para))  (cadr nestedExp))))   )) 
  )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    combineVarVal             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define combineVarVal
  (lambda (list1 list2)
  (cond ((null? list1) list1)
      ((eq? (length list1) 1) (list (append list1 list2 '())))
  (else (cons (list (car list1) (car list2)) (combineVarVal (cdr list1) (cdr list2)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  eliminate-nested-defines    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eliminate-nested-defines
  (lambda (exp)
    (letrec ((run (lambda (new-exp)

                  (cond ((null? new-exp) new-exp)
                        ((not(list? new-exp)) new-exp)                        
                        ((or (equal? (car new-exp) 'lambda-simple) (equal? (car new-exp) 'lambda-opt) (equal? (car new-exp) 'lambda-var))              
                          (let* ((new-body (run (get-lambda-body new-exp))))                                
                           ( eliminate-nested-defines1  (set-lambda-body new-exp new-body) )))                      

        (else  (map run new-exp)) ))))
    (run exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  eliminate-nested-defines1   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eliminate-nested-defines1
  (lambda (exp)
    (let 
      ((nested_exp 
       (if (eq? (car exp) 'def)
      (eliminate-nested-defines-begin (cdaddr exp) (lambda (x y)(list x (cdr y))))  
      (eliminate-nested-defines-begin (cdr exp) (lambda (x y)(list x (cdr y)))))))

     (cond 
           ((null? (car nested_exp)) exp)
           ((eq? (car exp) 'def) 
            (let* (   (lambdaExp (caddr exp))
                      (variables (cadr lambdaExp)))
                `(def  ,(cadr exp)  ,(append (parse-letrec (make-letrec lambdaExp (car lambdaExp) variables ))))            
            ))
            
            ( (is-it-lambda?  exp ) 
                (parse-letrec
                   (make-letrec 
                      (set-lambda-body exp (eliminate-nested-defines1 (get-lambda-body exp))) 
                      (car exp) 
                      (get-lambda-parameters exp))))
                           

              )
   
           ))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Remove-applic-lambda-nil    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define remove-applic-lambda-nil
     (lambda (exp)
        (letrec ((run (lambda (exp) 

                                (cond ((null? exp) exp)
                                ((not(list? exp)) exp)
                                ((and (eq? (car exp) 'applic) (eq? (caadr exp) 'lambda-simple) (null? (cadadr exp)) (= (length exp) 3) (null? (caddr exp)) )                     
                                         (let ((newExp (cddadr exp))) 
                                         (run (car newExp))  ))
                  (else (map run exp))) 
                              )))
      (run exp))
      ))
                               





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Part 5:                      						              	 ;;
;;      Boxing of variables                 				         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  checkAccurance       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define checkAccurance
  (lambda (exp v)
    (cond ((null? exp) #f)
               ((not(list? exp)) #f)
			   ((and (is-it-lambda? exp) (member v (get-lambda-parameters exp) )) #f)

                ((equal? (list 'var v) exp) #t)                                              
                  (else (ormap (lambda (x)(checkAccurance x v)) exp)))))

(define is-it-really-bound?
  (lambda (exprsion variable)
    (letrec  ((run (lambda (exp var)             
				         (cond ((null? exp) #f)
				                ((not(list? exp)) #f)
				                ( (and (is-it-lambda? exp) (not (member var (get-lambda-parameters exp) ))) (checkAccurance exp var))
				                ( (and (is-it-lambda? exp)  (member var (get-lambda-parameters exp) )) #f)				                				                                      
				                  (else (ormap (lambda (x)(run x var)) exp))))))
		(run exprsion variable ) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  is-it-bound?         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define is-it-bound?
  (lambda (exp var)
         (cond ((null? exp) #f)
               ((not(list? exp)) #f)
                ((is-it-lambda? exp)  (is-it-really-bound? (get-lambda-body exp)  var) )
                  (else (ormap (lambda (x)(is-it-bound? x var)) exp)))))
                                               
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  checkSet             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define checkSet 
  (lambda (exp var) 
   ;
      (cond ((null? exp) #f)
                ((not(list? exp)) #f)
			   ((and (is-it-lambda? exp) (member var (get-lambda-parameters exp) )) #f)

                ((and(equal? (car exp) 'set)(equal? (cadr exp) (list 'var var))) #t)                
                  (else (ormap (lambda (x)(checkSet  x var)) exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  nested-set-variable? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nested-set-variable?
    (lambda (exp var)
      (cond ((null? exp) #f)
            ((not(list? exp)) #f)
            ((and (is-it-lambda? exp) (not (member var (get-lambda-parameters exp) )))(checkSet exp var))
			((and (is-it-lambda? exp)  (member var (get-lambda-parameters exp) )) #f)
                  (else (ormap (lambda (x)(nested-set-variable? x var)) exp)))))






				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  nested-set-variable-in-body?     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nested-set-variable-in-body?
    (lambda (exp var)
	(letrec ((newExp (lambda (exp1)                    
							 (filter (lambda (x)(not(is-it-lambda? x))) exp1)) )
	         (run (lambda (expr var)
                   (cond  ((null? expr) #f)
                          ((not(list? expr)) #f)
                          ((not (is-it-lambda? expr)) (checkSet expr var))
                          ((and (is-it-lambda? expr)  (member var (get-lambda-parameters expr))) #f)	
                          (else (ormap (lambda (x)(run x var)) expr))))))
		    (run (newExp exp) var))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  has-set!-expr?     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define has-set!-expr? 
  (lambda (exp var) 
      (cond ((null? exp) #f)
               ((not(list? exp)) #f)
                ((is-it-lambda? exp ) 
				(or (nested-set-variable-in-body? (get-lambda-body exp) var)
				     (nested-set-variable? (get-lambda-body exp) var)))                                               
                  (else (ormap (lambda (x)(has-set!-expr? x var)) exp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  has-get-occurence?   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define has-get-occurence? 
  (lambda (exp var) 
      (cond ((null? exp) #f)
                ((not(list? exp)) #f)
                ((and( equal? (car exp) 'set)(equal? (cadr exp) (list 'var var))) (has-get-occurence? (caddr exp) var ) )
                ((equal? (list 'var var)  exp ) #t)                                              
                  (else (ormap (lambda (x)(has-get-occurence?  x var)) exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Should-we-box-it?    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define should-we-box-it?
  (lambda (exp var)
    (and (is-it-bound? exp var) (has-set!-expr? exp var ) (has-get-occurence? exp var) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     bounded-vars      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bounded-vars
   (lambda (exp vars) 
   
    (filter (lambda (x) 
      (should-we-box-it? exp x)) vars)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    make-box-per-var   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-box-per-var
   (lambda (exp var) 
    (cond ((null? exp) exp)
            ((not(list? exp)) exp)
            ((and (is-it-lambda? exp) (member var (get-lambda-parameters exp)))
                (set-lambda-body exp    (eraseNestedSeq (list 'seq (list `(set (var ,var)  (box (var ,var))) (get-lambda-body exp)))) ) )

                          
                 (else (map (lambda (x)(make-box-per-var x var)) exp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     make-box          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-box
   (lambda (exp vars) 
    (if (null? vars) exp
    (make-box (make-box-per-var exp (car vars)) (cdr vars)))))




;;;;;;;;;;;;;;;;;;
;;  Box-setter  ;;
;;;;;;;;;;;;;;;;;;
(define box-setter
  (lambda (exp var)
    (cond ((null? exp) exp)
        ((not (list? exp)) exp)
        ((and (is-it-lambda? exp)  (member var (get-lambda-parameters exp)) )   exp )  

        ((and (equal? (car exp) 'set) (equal? (list 'var var) (cadr exp)))
           	`(box-set ,@ (box-setter (cdr exp) var)))
           
         (else (map (lambda (x) (box-setter x var)) exp)))))
                
(define make-box-setter
   (lambda (exp vars) 
    (set-lambda-body exp (box-setter (get-lambda-body exp ) vars ))))


 


;;;;;;;;;;;;;;;;;;
;;  Box-getter  ;;
;;;;;;;;;;;;;;;;;;
(define box-getter
  (lambda (exp var)
   
    (cond ((null? exp) exp)
        ((not (list? exp)) exp)
        ((and (is-it-lambda? exp)  (member var (get-lambda-parameters exp)) ) exp ) 
        ((equal? 'box-set (car exp)) `(box-set ,(cadr exp) ,@(box-getter (cddr exp) var)))
         ((and (equal? exp (list 'var var)))
            `( box-get ,(list 'var var)))
         (else (map (lambda (x) (box-getter x var)) exp)))))
                

(define make-box-getter
   (lambda (exp var) 
    (set-lambda-body exp (box-getter (get-lambda-body exp ) var ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  box-set-get-per-var ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define box-set-get-per-var
  (lambda (exp var) 
    (let* ( ;(set-box-exp (box-setter  exp var))
           (set-box-exp (make-box-setter  exp var))
          (get-box-exp (make-box-getter set-box-exp var))
          (make-box-exp (make-box-per-var get-box-exp var)))
      make-box-exp))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Box-set ___final    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define box-set
  (lambda (exp)
     (letrec ((run (lambda (exp)
      (cond ((null? exp) exp)
            ((not (list? exp) )exp )
            ((is-it-lambda? exp) 
 
            (let* ((vars (reverse (bounded-vars exp (get-lambda-parameters exp)))))                    
               (if (not(null? vars)) 
               (letrec ((insideOut (lambda (new-exp lst-bound-vars)
                                (if (null? lst-bound-vars) 
                                new-exp
			      (insideOut (box-set-get-per-var
				 (set-lambda-body new-exp (run (get-lambda-body new-exp)))(car lst-bound-vars))(cdr lst-bound-vars))
                
                               ))))

                           (insideOut exp vars))

              (set-lambda-body exp (run (get-lambda-body exp) ))
             )))
        (else 
          (map (lambda (x)(run x) )exp) 
          ) ) )))
     (run exp)
  )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Part 6:                                                                 ;;
;;      Annotating Variables with their Lexical address                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    minor-lookup      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define minor-lookup 
  (lambda (var env)
    (let ((location (member var (reverse env))))
      (if location 
        (- (length location ) 1)
        -1 ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    major-minor       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define major-minor 
  (lambda (var lst) 
    (letrec ((bingo (lambda (var lst counter)
             
              (if (null? lst)
                 #f
            (let ((minor (minor-lookup var (car lst))))
              (if (> minor -1)
                `(bvar  ,var ,counter ,minor)
              (bingo var (cdr lst) (+ 1 counter))))))))
  (bingo var lst 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  annotate-variables  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define annotate-variables
  (lambda (var args env)
    
    (if (member var args) 
      (let ((minor (minor-lookup  var args)))
        `(pvar ,var ,minor)  )
        (let ((major (major-minor var env)))
          (if major 
            major
          `(fvar ,var))))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  traverse-tree       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define traverse-tree 
  (lambda (exp  param env) 
    (letrec ((run (lambda (exp param env)
                    (cond ((null? exp) exp)
                          ((not (list? exp)) exp)
                          ((list? (car exp)) (cons ( traverse-tree (car exp) param env) ( traverse-tree (cdr exp) param env)))
                          ((eq? (car exp) 'lambda-simple) (with exp (lambda (tagName args body) `( lambda-simple ,args ,(run body args (cons param env) )) )))
                          ((eq? (car exp) 'lambda-var)  (with exp (lambda (tagName arg body )  `(lambda-var ,arg ,(run body (list arg) (cons param env) )) )))
                          ((eq? (car exp) 'lambda-opt) (with exp (lambda (tagName args optArg body )`(lambda-opt ,args ,optArg ,(run body (append args (list optArg))  (cons param env) )) )))

                                                             ((eq? (car exp) 'var) (annotate-variables  (cadr exp) param  env))
                                                             (else (map (lambda (x) (traverse-tree x param env)) exp))))))
                  (run exp param env)    
                  )))



;;;;;;;;;;;;;;;;;;;;;;
;;  Pe->lex-pe      ;;
;;;;;;;;;;;;;;;;;;;;;;
(define pe->lex-pe
  (lambda (exp) (traverse-tree  exp '() '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Part 7:                                 ;;
;;      Annotating tail calls               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      annotate               	   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define annotate
    (lambda (expr tp?)
            
        (cond 
            
              ((and (equal? (car expr) 'applic) tp?) `(tc-applic ,(annotate (cadr expr) #f) ,(map (lambda (x)
                                            (annotate x #f)) (caddr expr))))
   
              ((and (equal? (car expr) 'applic)(not tp?) ) `(applic ,(annotate (cadr expr) #f) ,(map (lambda (x)
                                            (annotate x #f)) (caddr expr))))
              ((or (equal? (car expr) 'const) (equal? (car expr) 'var) (equal? (car expr) 'pvar) 
                   (equal? (car expr) 'bvar) (equal? (car expr) 'fvar)) expr)
                    
              ((equal? (car expr) 'or)
                            `(or ,(map (lambda (x)
                       (if (eq? (car (list-tail (cadr expr) (- (length (cadr expr)) 1))) x)
                        (annotate x tp?)
                                                (annotate x #f))) 
                                        (cadr expr))))
               ((equal? (car expr) 'if3)
                            `(if3 ,(annotate (cadr expr) #f)
                                   ,(annotate (caddr expr) tp?)
                                   ,(annotate (cadddr expr) tp?)))
               ((equal? (car expr) 'def)
                            `(def ,(cadr expr) ,(annotate (caddr expr) #f)))

               ((equal? (car expr) 'lambda-simple)
                            `(lambda-simple ,(cadr expr) ,(annotate (caddr expr) #t)))
               ((equal? (car expr) 'lambda-opt)
                            `(lambda-opt ,(cadr expr) ,(caddr expr) ,(annotate (cadddr expr) #t)))
               ((equal? (car expr) 'lambda-var)
                            `(lambda-var ,(cadr expr) ,(annotate (caddr expr) #t)))

                  ;;;;;;;;box;;;;;;;;

               ((equal? (car expr) 'box-get) expr)

               ((equal? (car expr) 'box-set)
                        `(box-set ,(cadr expr) ,(annotate (caddr expr) #f)))
               ((equal? (car expr) 'box)
                      `(box ,(annotate (cadr expr) tp?)))
               ((equal? (car expr) 'set)
                        `(set ,(cadr expr) ,(annotate (caddr expr) #f)))
               (else (equal? (car expr) 'seq)
                            `(seq ,(map (lambda (x)
                     (if (eq? (car (list-tail (cadr expr) (- (length (cadr expr)) 1))) x) 
                                                (annotate x tp?)
                                                (annotate x #f)))
                                            (cadr expr))))
                )))
                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      annotate-tc            	   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define annotate-tc
    (lambda (expr)
        (annotate expr #f)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Final Project:                                                                               ;;
;;                                                                                             ;;
;;                                                                                             ;;
;;                                                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "constant-table.scm")
;free-var-table
;constant-table
(define list-of-exps 
  (lambda (string )
    (<sexpr> (string->list string)
      (lambda (e s)
        (append (list e)
    (list-of-exps (list->string s))))
      (lambda (w) `()))))

(define optimization 
    (lambda (expr)
      (annotate-tc
          (pe->lex-pe
              (box-set
                (remove-applic-lambda-nil 
                (eliminate-nested-defines expr)))))))

(define const->string
    (lambda (const)
        (cond  ((string? const) const)
		       ((number? const) (number->string const)) 
		       (else(symbol->string const)))))

(define flatten 
  (lambda (lst)
  (cond ((null? lst) '())
        ((pair? lst) (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst)))))
(define our-map
  (lambda (proc items)
    (if (null? items)
    (list)
    (cons (proc (car items))
    (our-map proc (cdr items))))))

;(testShow "after BLAAA")
(define testShow ;			


    (lambda (to-print )

            (string-append
                "SHOW(\"" 

                to-print 
                "\"" ",R0);\n"
                ;","who"("(number->string from)"));\n"
                ;	"SHOW(\"BLAAA\" ,R0);\n"

                
            "")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Prologue                                  ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define prologue 
  (lambda ()
   (string-append "
/* cisc.c
* Mock-assembly programming for a CISC-like architecture
* 
* Programmeres: Yossi Cherniak &  Or Naimark, 2017 
*/

#include <stdio.h>
#include <stdlib.h>
/* change to 0 for no debug info to be printed: */
#define DO_SHOW 0

#include \"cisc.h\"
#include \"debug_macros.h\"   /*to delete*/

int main()
{
START_MACHINE;
  JUMP(CONTINUE);\n"

  "#define SOB_VOID\t"    (number->string (get-constant-address *void-object*)) "\n"
  "#define SOB_NIL\t"     (number->string (get-constant-address '())) "\n"
  "#define SOB_TRUE\t"    (const->string (get-constant-address '#t))"\n"
  "#define SOB_FALSE\t"   (const->string (get-constant-address '#f))"\n"



"#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"
#include \"scheme.lib\"

#include \"primitivies_library.asm\"

  

  CONTINUE:\n" )  
   ) )
;"#define SOB_VOID  "  (number->string 100) "\n"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Epilogue                                  ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define epilogue 
  (string-append "
  JUMP(EXIT);

  ERRORS:
  APPLIC_NOT_A_CLOUSER_ERROR:
  SHOW(\"ERROR: Applic_not_a_closure_error11!!!\" ,R0);
  INFO;
  JUMP(EXIT);
  
  L_INCORRECT_NUMBER_OF_ARGS_ERROR:
  SHOW(\"ERROR: INCORRECT_NUMBER_OF_ARGS_ERROR!!!\" ,R0);
  INFO;
  JUMP(EXIT);
  
  L_INCORRECT_TYPE:
  SHOW(\"ERROR: Not correct type\" ,R0);
  INFO;

  JUMP(EXIT);
  
  L_ERROR_INCORRECT_TYPE_ERROR_NOT_PAIR:
  SHOW(\"Type Not a pair\" ,R0);
  INFO;
  JUMP(EXIT);

  L_INCORRECT_TYPE_STRING:
  SHOW(\"ERROR: Wrong Type.Type Not a STRING!!!!\" ,R0);
  JUMP(EXIT);


  EXIT:
    STOP_MACHINE;

  return 0;

} 
  ") )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Code-Generator                            ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "code-gen.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;librar                                    ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(scheme_func_name . "label of the function")
(define prim-table

	   '( 
   (car . "L_Car") (cdr . "L_Cdr") 
   (boolean? . "IS_Boolean") 
      (void? . "IS_Void")
    (null? . "IS_Nil")
      (char? . "IS_Char")
    (integer? . "IS_Integer")
    (string? . "IS_String")
 (symbol? . "IS_Symbol")(pair? . "IS_Pair")(vector? . "IS_Vector")
    (procedure? . "IS_Closure")  (zero? . "IS_Zero") (cons . "MAKE_CONS") (set-car! . "SET_CAR")(set-cdr! . "SET_CDR")
	 (char->integer . "CHAR_TO_DIGIT")(integer->char . "DIGIT_TO_CHAR")(vector-length . "VECTOR_LENGTH") (vector-ref . "VECTOR_REF") (vector-set! . "VECTOR_SET")
	 (vector . "VECTOR") (string-length . "STRING_LENGTH")(string-ref . "STRING_REF")(string-set! . "STRING_SET")
	  (binary-add . "BINARY_ADD")
    (binary-sub . "BINARY_SUB") 
    (binary-mul . "BINARY_MUL") (binary-div . "BINARY_DIV") (binary-eq . "BINARY_EQ") (binary-gt . "BINARY_GT") 
    (binary-lt . "BINARY_LT")
	(numerator . "NUMERATOR") 	 
  (denominator . "DENOMINATOR") ; this is in imm(100)
   (rational? . "IS_RATIONEL") 
 (number? . "IS_NUMBER") (remainder . "REMAINDER")(symbol->string . "SYMBOL_TO_STRING") (eq? . "TYPES_IS_EQ") 
 (make-string . "MAKE_STRING")
 (make-vector . "MAKE_VECTOR") (apply . "APPLY") 
 ;(string-compare  . "STRING_COMPARE")
 (string->symbol . "STRING_TO_SYMBOL")

))



(define primitivies-lib-table->cisc
  (lambda () 
    (fold-left
     (lambda (acc el)
       (let* ((index (number->string (get-free-var-address (car el) ))))
         (string-append 
          acc    
          "L_make_" (cdr el)  "_Clos:\n"
          "PUSH(IMM(3));\n"
         ; "SHOW(\"first malloc!!!\" ,R0);\n"

          "CALL(MALLOC);\n"
          "DROP(1); \n"
          "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
          "MOV(INDD(R0,1),IMM(0XFF)); /*env*/\n"
          "MOV(INDD(R0,2),LABEL("(cdr el)"));\n"
          ;R0 <- address of a closure
          "MOV(IND(" index"),R0);\n\n\n" )))
     ;move label address to table
     ""  prim-table)
    ))

                  
                  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          compile-scheme-file             ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compile-scheme-file
  (lambda (input-name output-name )
    (let* (;(out-port (open-output-file (symbol->string output-name) 'truncate))
            (out-port (open-output-file  output-name 'truncate))
          (parsed-file-user ( map optimization (map parse (list-of-exps (file->string input-name)))))
		  (scheme-primitives-file ( map optimization (map parse (list-of-exps (file->string "primitives_in_scheme.scm")))))
		  (parsed-file (append scheme-primitives-file parsed-file-user ))

          (constants (begin (const-collector parsed-file ) 
                            (remove-const-dups   )
                         ;   (constant-table-reset)
                           
                            (make-constant-table)

                              (constant-table->cisc constant-table) ))

          (free-vars (begin  (free-var-collector parsed-file)
                            (remove-free-dups )
                          ;  (free-var-table-reset)
                            (make-free-var-table)
                            (free-var-table->cisc free-var-table)
                          ))
          (symbols (begin (make-symbol-table constant-table) (fix-tables (symbol-table->cisc) )	))


          (gen-sexprs (map (lambda (el) (string-append (code-gen el -1) (code-gen-GUL) )) parsed-file ))
          (end-address next-address)
          )
    
      ;body
      (display (prologue) out-port)
      ;display constant table cisc
      (display (string-append"/********************  START-Const-table ********************/\n"  constants  "/********************  End-Const-table  ********************/\n\n")  out-port)
      ;display fvar-table-cisc
      (display (string-append "/********************  START-Free-var-table ********************/\n" free-vars  "/********************  End-Free-var-table  ********************/\n\n")  out-port)

      ;symbol-table-on compile time
      (display (string-append "\n/********************  START-Symbol-table-compileTime ********************/\n" "MOV(R12," (number->string (head-address)) ");\n"	symbols  "/********************   End-Symbol-table-compileTime  ********************/ \n\n\n")  out-port)
     
      ;basic fvar-table
      (display (string-append "/********************  START-Library ********************/\n"  "MOV(IND(0),"(number->string end-address )");\n" ;"INFO;\n"

        (primitivies-lib-table->cisc)  "/********************  End-Library  ********************/\n\n\n\n\n")  out-port)

             (display  "/********************  START-Gen-Sexprs ********************/\n" out-port)

      (for-each (lambda (expr) (display expr out-port) (display "\n" out-port))  gen-sexprs )
             (display  "/********************  END-Gen-Sexprs ********************/\n" out-port)


      	(display epilogue out-port)
      ;parsed-file) out-port ) 
        (close-output-port out-port)))
      )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
