(define empty-out
    (lambda (lst)
	   (filter (lambda(p)
	             (not (null? (caddr p)))) lst)))
				  

(define bla   '(  ('g46494 (6 1) (9))
            ('g46495 (3 6 0) (0 4))
            ('g46496 (1) (3))
            ('g46497 (9) (4 2))
            ('g46498 (7 1 3) (9)) 
            ('g46499 (7) (9))))	
(define bla2  (reverse (cdr (reverse'(  ('g46494 (6 1) (9))
           ('g46495 (3 6 0) (0 4))
            ('g46496 (1) (3))
            ('g46497 (9) (4 2))
            ('g46498 (7 1 3) (9)) 
            ('g46499 (7) (9)))))))				

(define changeList
    (lambda (lst value)
	(let ((helper 0))
              (filter (lambda (exp) 
	                           (cond ((and (= helper 0)(member (car value) (cadr exp)) (not (equal? (caddr exp) value))) (begin (set! helper 1) #t))
                                     ((and (= helper 1) (equal? (caddr exp) value)) (begin (set! helper 0) #t))
                                     ((= helper 0) (not (equal? (caddr exp) value)))									 
			                          (else #t)))
			            lst))))

			  
(define changeListUp
   (lambda (lst)
      (letrec ((run 
                 (lambda (s)
			     	(if (null?  s) s
				        (let* ( (firstArg (car (reverse s)))
						        (value (caddr firstArg))
								(workinglist  (if(null? (cdr s)) s (reverse (cdr (reverse s)))))
						        (newList (changeList workinglist value)))
				         (append (list firstArg)(run newList)))))))
						 
					 (run lst) )))
					 
(define remww
   (lambda (lst)					 
         (let* ((first (empty-out lst))
                (second  first)
                (third    (changeListUp second)))
                (reverse third))))				