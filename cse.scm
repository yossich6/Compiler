(print-gensym #f)

(define listOflist?
	(lambda (expr) 
		(ormap (lambda (x) (and (list? x) (not (quote? x) ))) expr)))



(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))

(define count-occurrences
  (lambda (x ls)
    (cond 
    	  ((equal? x ls) 1)
    	  ((null? ls) 0)
    	  ((not (list? ls) ) 0)
       (else (fold-left + 0 (map (lambda (smallerLs)(count-occurrences x smallerLs)) ls))))))




(define reallyHasDups?
     (lambda (arg lst)
	    (cond  ((null? lst) #f) 
	           ((not (list? lst)) #f)
	           ((not (listOflist? lst)) #f)
	           ((not (list? arg) ) #f)
	       	   ((quote? arg ) #f)
	           ((> (count-occurrences arg lst) 1) arg)
	    (else (reallyHasDups? arg (cdr lst)))    
                  )))

(define hasDups?
	(lambda (orignalList lst)
	;	(display lst) (newline)
		(cond ((not (list? lst) ) #f)
			  ((null? lst ) #f) 
			  ((and (list? (car lst)) (not (quote? (car lst) )) (listOflist?  (car lst)))
			  		 (hasDups? orignalList (car lst) )) 
			 ((reallyHasDups? (car lst) orignalList) (reallyHasDups? (car lst) orignalList))
			 (else (hasDups? orignalList (cdr lst))))))





(define setChangedList
    (lambda (bigExp multyExp optimizeExp)
        (cond 
              ((null? bigExp) bigExp)
              ((equal? bigExp multyExp) optimizeExp)
              ((not (list? bigExp)) bigExp)
              (else (map (lambda (exp) (setChangedList exp multyExp optimizeExp)) bigExp)))))


(define setChanges 
	(lambda (changes multy gensymVar)
	            (cons (list gensymVar multy) changes)))



(define optimizeList 
	(lambda (orignalList changedList changes)
			(if (hasDups? changedList changedList) 
                (let ((gensymVar (gensym))
                      (multy 	(hasDups? changedList changedList))) 

                    (optimizeList    orignalList
				                    (setChangedList changedList multy gensymVar) 
			 	                    (setChanges     changes multy gensymVar )))


		    (cons changes changedList))

          ))
(define best? 
	(lambda (arg lst)
		(let ((count  (count-occurrences arg lst) )) 
		(= count  2)) 
			) )
(define getGen
	(lambda (changes) (caar changes) ))

(define getSecondElment
	(lambda (opt) (cadr opt )))

(define combineList 
		(lambda lst
			lst))
		 	        


(define removeChange
		(lambda (opt firstChange)
		

		;setChangedList
		 (let (( ans (setChangedList opt (car firstChange) (getSecondElment firstChange ))))
			( combineList (filter (lambda (pair) (not (equal? (car pair) (getSecondElment pair)))) (car ans))  (getSecondElment ans)))))



			
;;;
(define bestFit
	(lambda (changes changedList)
		(let ((opt (list changes changedList)) )
			
		(if (null? changes)  
			opt   ;we found the bestFit 
			(if (best? (getGen changes) opt)   ; there is an unnecessary optimization 
				 ;make new optimization
				 (let*  ((betterOpt  (removeChange opt (car changes ) )))
				 		  ;(display betterOpt) (newline)	 
				 	     (bestFit (car betterOpt)  (getSecondElment betterOpt) ))

				 ;else ,Check if rest of changes have an unnecessary optimization
				 (let*   ((bestOpt (bestFit (cdr changes) changedList ))
				 		  (first  (append (list (car changes ) ) (car bestOpt)))
				 		  (sec    (getSecondElment bestOpt)))
				 	(list  first sec )))))
	))
;;
(define cse
	(lambda (expr)
		(let ((originExp expr)
			  (changes '()))
		    (let* ((opt (optimizeList originExp originExp changes))
		           (vars (reverse (car opt)))
		           (newExp (cdr opt))
		       	   (bestOpt (bestFit  vars newExp )))
                  (if (equal? originExp newExp)
                  	originExp ;return not changed
                  (if (= 1 (length (car bestOpt)))
                  	`(let ,(car bestOpt) ,(getSecondElment bestOpt)) 
                  	`(let* ,(car bestOpt) ,(getSecondElment bestOpt))) )) 
      
		    )))






