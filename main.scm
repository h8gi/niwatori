;;; main.scm    source of niwatori.scm
(use matchable)
(import-for-syntax matchable)
(include "server.scm")

(define (header-ref key header)
  (alist-ref key header (lambda (x y)
			  (string-ci= (->string x)
				      (->string y)))))
(define (header-update key value header)
  (alist-update key value header (lambda (x y)
				   (string-ci= (->string x)
					       (->string y)))))

(define (send-response body-thunk
		       #!key (status 200) (reason "OK") (header '()))
  (let* ([body   (with-output-to-string body-thunk)]
	 [header
	  (cons `(content-length . ,(string-length body))
		(if (header-ref 'content-type header)
		    header
		    (header-update 'content-type "text/html" header)))])
    (printf "HTTP/1.1 ~A ~A~%" status reason)
    (display-header header)
    (display body)))

(define *action-table* (alist->hash-table '((get) (post) (head) (put) (delete))))
(define (init-action-table!)
  (set! *action-table* (alist->hash-table '((get) (post) (head) (put) (delete)))))

;;; action setter
;;; (proc mached-ref method header body)
(define (action-set! method path-irx action)
  (let ([table (hash-table-ref/default *action-table* method #f)])
    (if table
	(hash-table-set! *action-table*
			 method
			 (append table (list (cons path-irx action))))
	(error "Illegal method" method))))
;;; syntax sugar of action setter 
(define-syntax @
  (ir-macro-transformer
   (lambda (expr inject compare)
     (match expr
       [(_ method path-irx thunk opts ...)
        (let ([matched  (inject '$matched)]
	      [request  (inject '$request)]
	      [header   (inject '$header)]
	      [uri	(inject '$uri)]
	      [body	(inject '$body)])
	  `(action-set! ,method ,path-irx
			(lambda (,matched ,request ,header ,uri ,body)
			  (send-response ,thunk ,@opts))))]))))

(define (action-dispacher request)
  (let ([method	(request-method request)]
	[uri	(request-uri request)]
	[header (request-header request)]
	[body	(request-body request)])
    (define (inner table)
      (cond [(null? table)
	     (send-response (lambda () (display "404 not found"))
			    #:status 404
			    #:reason "Not Found")]
	    [else (let* ([pair (car table)]
			 [irx  (car pair)]
			 [action (cdr pair)]
			 [m (irregex-match irx uri)])
		    (if m
			(action (lambda (name-or-index)
				  (irregex-match-substring m name-or-index))
				request
				header
				uri
				body)
			(inner (cdr table))))]))
    (inner (hash-table-ref/default *action-table* method '()))))

(*server-proc* action-dispacher)
