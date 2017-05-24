 


(define next-address 1);like our grade in the compiler

(define remove-duplicates 	
	(lambda  (lst) 
		 (fold-left 
		 	(lambda (acc elm) 
				(if (not (member elm acc))
						(append acc (list elm)) 
						acc) ) '() lst)))	



(define getAddrAndInc 
	(lambda (n) 
			(let ((old-addr next-address))
			(set! next-address (+ next-address n))
			 old-addr)))

(define get-value-address
	(lambda (value lstValAdd)
	;(display lstValAdd) 
		(cond ((null? lstValAdd) -1)
			  ((null? (cdr lstValAdd)) (caar lstValAdd))
			  ((equal? (cadr (car lstValAdd)) value) (caar lstValAdd))
			  (else (get-value-address value (cdr lstValAdd)))
			
		)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant-table                           ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define constantList `(,*void-object*  ()  #t #f) ) ;not-to use fake!!!

(define constant-table `(,*void-object*  ()  #t #f) )

(define constant-table-reset
   (lambda ()
	(set! constant-table `(,*void-object*  ()  #t #f))))

(define const-collector 
	(lambda (exp) 
		(cond ((or (null? exp) (not (list? exp)))  '() )
			  
			  ((equal? (car exp) 'const )
			  	(cond ((vector? (cadr exp))  
			  			 (begin (map (lambda (element) (const-collector `(const  ,element))) (vector->list (cadr exp)))
			  			 		(set! constantList (append constantList (list (cadr exp)) ))))
			  			( (or (null? exp) (null? (cdr  exp)) )'())
			  			((pair? (cadr exp)) 
				  			(begin  (const-collector `(const ,(caadr exp)) )  ;car
				  					(const-collector `(const ,(cdadr exp)) )  ;cdr
				  					(set! constantList (append constantList (list (cadr exp) ))))) ;pair itself
			  			
			  			((string? (cadr exp))
			  				(set! constantList (append constantList (list (cadr exp)))))
			  			((symbol? (cadr exp))
			  			    		(begin (set! constantList (append constantList (list (symbol->string (cadr exp)))))
			  			    		       (set! constantList (append constantList (list (cadr exp))))))

 
			  	 (else  (set! constantList (append constantList (list (cadr exp))))) ))
			  (else (map const-collector exp))
			)))

(define remove-const-dups 
	(lambda ()
		(set! constantList (remove-duplicates constantList) )))
		
(define get-constant-address
	(lambda (const)
		 (get-value-address const constant-table)))

(define get-constant-address2
	(lambda (const new-lst) 
			(get-value-address const new-lst)))
 	
(define const-type 
		(lambda (c new-lst) 
			(cond ((equal? c *void-object* ) `(,(getAddrAndInc 1)  ,c T_VOID ) )
				  ((equal? c '() ) `(,(getAddrAndInc 1)  ,c T_NIL ) )
				  ((or (equal? c #t) (equal? c #f))  
				  	(if c   `(,(getAddrAndInc 2) ,c (T_BOOL 1) )
				  			`(,(getAddrAndInc 2) ,c (T_BOOL 0) )))
				  ((integer? c) 
				  	`(,(getAddrAndInc 2) ,c (T_INTEGER ,c)))

				((pair? c) 
					(let ((first (get-constant-address2 (car c) new-lst) )
						  (second (get-constant-address2 (cdr c) new-lst)) )
				  	`(,(getAddrAndInc 3) ,c (T_PAIR ,first ,second))))

				((vector? c)
						(let* ( (vec-len (vector-length c))
								(items-address (map (lambda (el) (get-constant-address2 el new-lst)) (vector->list c))))
							`(,(getAddrAndInc (+ 2 vec-len )) ,c (T_VECTOR ,vec-len ,@items-address ))))
				((rational? c) `(,(getAddrAndInc 3) ,c (T_FRACTION ,(numerator c) 
				                    ,(denominator c))))


				  ((char? c)
				  	`( ,(getAddrAndInc 2) ,c (T_CHAR ,(char->integer c) )))

				  ;string
				  ((string? c)
				  	(let* ((str-len (string-length c ))
				  			(item-ascii (map (lambda (el) (char->integer el)) (string->list c)) ))
				  	`(,(getAddrAndInc (+ 2 str-len)) ,c (T_STRING ,str-len ,@item-ascii)  )))
				  ;symbol
				  ((symbol? c)
				  	(let ((strAddress (get-constant-address2 (symbol->string c) new-lst ) ))
				  	`(,(getAddrAndInc 2) ,c (T_SYMBOL ,strAddress) )))
				  ;!!!fraction !!! T_FRACTION to do!!!!

				  
				(else  (display c) -8 ))))


;run on list and make data item ( (address c type) ...) 
;example   ((110 1 (T_INTEGER 1)) (112 3 (T_INTEGER 3)) (120 (1 . 3) (T_PAIR 110 112) ) ... )
(define make-constant-table
	(lambda ()
			(letrec ((new-table 
					(lambda (const-lst new-lst)
							(if (null? const-lst) new-lst
							    (new-table (cdr const-lst)  (append new-lst (list (const-type (car const-lst) new-lst) )) ))
							)))
		(set! constant-table  
			(new-table constantList '() ) ))))


;(T_VOID T_NIL (T_BOOL 1) (T_BOOL 0) (T_STRING 4 97 112 112 108) (T_SYMBOL 106) (T_INTEGER 2))
;(T_VOID T_NIL T_BOOL 1 T_BOOL 0 T_STRING 4 97 112 112 108 T_SYMBOL 106 T_INTEGER 2)
(define make-type-list 
    (lambda (table)
      (append '(T_VOID T_NIL) (flatten (cddr (map append (map caddr table)))))))


(define constant-table->cisc
      (lambda (table)
          (let* ((type-table (make-type-list table))
                  (index (- (caar table) 1) )
                  
                  )
          (fold-left  
            (lambda (acc x) (set! index (+ index 1)) 
            (string-append acc "MOV(IND(" (number->string index) ")," (const->string x) ");\n" ))
             ""  type-table )
          
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Free-var-table                           ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define free-var-table '( car cdr boolean? void? null? char? integer? string? symbol? pair? vector? closure? zero? cons set-car! set-cdr!
;integer->char char->integer vector-length vector-ref vector-set! vector string-length string-ref string-set! binary-add binary-sub binary-mul ;
(define free-var-table '( 
	car
  cdr 
	boolean? 
	void? 
	null? char? 
	integer? 
	string? 
	symbol? pair? vector? procedure? zero? cons set-car! set-cdr!
   integer->char char->integer vector-length vector-ref vector-set! vector string-length string-ref string-set! 
  binary-add binary-sub binary-mul binary-div binary-eq binary-lt binary-gt 
 numerator denominator rational? 
 number? remainder symbol->string eq? 
					  make-vector make-string apply string->symbol 
					  ;string-compare
) )

(define free-var-table-reset
   (lambda ()
	(set! free-var-table '() )))

(define add-to-free-var-table
    (lambda (fvar)
      (set! free-var-table (append free-var-table  fvar ))))

(define free-var-collector
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp)) ) '())
              ((equal? (car exp) 'fvar) (add-to-free-var-table (cdr exp))) 
                        (else (map free-var-collector exp))) ))
;free-var item -> (address value  undifined )
(define free-var-type 
      (lambda (c new-lst) 
          `(,(getAddrAndInc 1) ,c  0XFF) ))

(define remove-free-dups 
	(lambda ()
		(set! free-var-table (remove-duplicates free-var-table) )))

(define make-free-var-table
  (lambda ()
      (letrec ((new-table 
          (lambda (free-var-lst new-lst)
              (if (null? free-var-lst) new-lst
                  (new-table (cdr free-var-lst) 
                   (append new-lst (list (free-var-type (car free-var-lst) new-lst) )) ))
              )))
    (set! free-var-table  
      (new-table free-var-table '() ) ))))


(define get-free-var-address
	(lambda (fvar)
		 (get-value-address fvar free-var-table)))

	




(define free-var-table->cisc
      (lambda (table)
      	(if (null? table) "//NO Free vars \n" 
          (let* (
                  (index (- (caar table) 1) )
                  
                  )
          (fold-left  
            (lambda (acc x) (set! index (+ index 1)) (string-append acc "\tMOV(IND(" (number->string index) "), 0XFF );\n" ))
             ""  table ))
          
        )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol-table 	                         ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define get-String-address-in-const-table
		(lambda (element)
			(with element (lambda (addr c type )  (if (and (list? type) (equal? (car type) 'T_STRING)  ) addr #f)))))


(define symbol-table '()	)

;current =adress of current node 
;str-address =  string Address
;next =adress of next node. 0 means its last one
;(current str-address)
;(define make-node	)

(define make-symbol-table
	(lambda (table)
		(set! symbol-table
			(let ((new-table  table))
			 	 	(map car (filter get-String-address-in-const-table  new-table) ) ) )
			 	 	 symbol-table)
	) 

;R12

(define next-node-adress
	(lambda (lst val)	
			(if (= (car (reverse lst)) val) 0  next-address )
		))


(define symbol-table-test "")



;symbol table for debug
; ((address AddressString1 ) (address AddressString2 ) ...)
;((116 7) (118 14) ...) =>in address 116 z
(define temp-lst '())

(define symbol-table->cisc
  (lambda ()
    (if (null? symbol-table) "//NO Symbols in compile time\n"       		         
       (begin (set! symbol-table-test
              (fold-left  
               (lambda (acc x) ;(set! index (+ index 1))
                 (let* ( 
                 		(node-adress  (getAddrAndInc 2)) 
                        (next-node-addr (next-node-adress symbol-table x) ))
                   (set! temp-lst (append temp-lst (list (list node-adress x))));DEBUG
	             
                   (string-append acc 
                                  "MOV(IND("  (number->string  node-adress) "), "(number->string x)" );\n" 
                                  "MOV(IND(" (number->string (+ 1 node-adress) )"), "(number->string next-node-addr )" );\n" ;next-address

                                  )))
               ""  symbol-table ) )symbol-table-test )
        )))


(define fix-tables
	(lambda (exp)
		(let ((old-table symbol-table))
	
		 (set! symbol-table temp-lst)
	 	(set! temp-lst old-table) ) exp) )

(define head-address
		(lambda () 
			(if (and (not (null? symbol-table) ) (list? symbol-table) (list? (car symbol-table) ) ) (caar symbol-table)  0 ))) 