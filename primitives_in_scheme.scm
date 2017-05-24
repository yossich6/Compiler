(define list (lambda x x))

(define not
(lambda (x)
   (if x #f #t)))

(define fold-left 
(lambda (f init seq) 
   (if (null? seq) 
       init 
       (fold-left f 
                  (f init (car seq) ) 
                  (cdr seq))))) 

 (define fold-right 
 	(lambda (f init seq) 
   (if (null? seq) 
       init 
       (f (car seq) 
           (fold-right f init (cdr seq)))))) 


 (define binary-append 
		(lambda (list1 list2)
	(cond ((null? list1) list2)
		  ((and (null? (cdr list1)) (symbol? list2)) (cons (car list1) list2))
          (else (cons (car list1) (binary-append (cdr list1) list2))) )))


(define append
	(lambda s
  (fold-left binary-append '() s)))


(define map
(lambda (proc items)
(if (null? items)(list)(cons (proc (car items))
(map proc (cdr items))))))

(define +
	(lambda s
  (fold-left binary-add 0 s)))



(define -
(lambda s
		(let ((init (car s))
		       (cdr-list (cdr s)))

		       (cond  ((null? s) s)
		              ((null? (cdr s)) (binary-mul -1 (car s)))
                    (else (fold-left binary-sub init cdr-list))))))

 
(define *
	(lambda s
  (fold-left binary-mul 1 s)))

(define /
	(lambda s
		(let ((init (car s))
		       (cdr-list (cdr s)))

		       (cond  ((null? s) s)
		              ((null? (cdr s)) (binary-div 1(car s)))
                    (else (fold-left binary-div init cdr-list))))))


(define =
   (lambda lst 
   (letrec ((run (lambda (ls)  
	             (cond   ((null? ls) #t)
		                 ((null? (cdr ls)) #t)
	                     ((not (binary-eq (car ls) (car (cdr ls)))) #f)
                         (else (run (cdr ls)))))))

                (run lst))))

(define >
   (lambda lst 
   (letrec ((run (lambda (ls)  
	             (cond   ((null? ls) #t)
		                 ((null? (cdr ls)) #t)
	                     ((not (binary-gt (car ls) (car (cdr ls)))) #f)
                         (else (run (cdr ls)))))))

                (run lst))))

(define <
   (lambda lst 
   (letrec ((run (lambda (ls)  
	             (cond   ((null? ls) #t)
		                 ((null? (cdr ls)) #t)
	                     ((not (binary-lt (car ls) (car (cdr ls)))) #f)
                         (else (run (cdr ls)))))))

                (run lst))))



(define cadr (lambda (z) (car (cdr z))))
(define cdar (lambda (z) (cdr (car z))))
(define cddr (lambda (z) (cdr (cdr z))))
(define caar (lambda (z) (car (car z))))
(define cadar (lambda (z) (car (cdr (car z)))))
(define cdaar (lambda (z) (cdr (car (car z)))))
(define cddar (lambda (z) (cdr (cdr (car z)))))
(define caaar (lambda (z) (car (car (car z)))))
(define caddr (lambda (z) (car (cdr (cdr z)))))
(define cdadr (lambda (z) (cdr (car (cdr z)))))
(define cdddr (lambda (z) (cdr (cdr (cdr z)))))
(define caadr (lambda (z) (car (car (cdr z)))))
(define cadaar (lambda (z) (car (cdr (car (car z))))))
(define cdaaar (lambda (z) (cdr (car (car (car z))))))
(define cddaar (lambda (z) (cdr (cdr (car (car z))))))
(define caaaar (lambda (z) (car (car (car (car z))))))
(define caddar (lambda (z) (car (cdr (cdr (car z))))))
(define cdadar (lambda (z) (cdr (car (cdr (car z))))))
(define cdddar (lambda (z) (cdr (cdr (cdr (car z))))))
(define caadar (lambda (z) (car (car (cdr (car z))))))
(define cadadr (lambda (z) (car (cdr (car (cdr z))))))
(define cdaadr (lambda (z) (cdr (car (car (cdr z))))))
(define cddadr (lambda (z) (cdr (cdr (car (cdr z))))))
(define caaadr (lambda (z) (car (car (car (cdr z))))))
(define cadddr (lambda (z) (car (cdr (cdr (cdr z))))))
(define cdaddr (lambda (z) (cdr (car (cdr (cdr z))))))
(define cddddr (lambda (z) (cdr (cdr (cdr (cdr z))))))
(define caaddr (lambda (z) (car (car (cdr (cdr z))))))



