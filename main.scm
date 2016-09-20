;;; main.scm    source of niwatori.scm
(use matchable)
(import-for-syntax matchable)

(include "server.scm")
(define (send-response body-thunk
		       #!key (status 200) (reason "OK") (header '()))
  (let* ([body   (with-output-to-string body-thunk)]
	 [header (append `((content-type . "text/html")
			   (content-length . ,(string-length body)))
			 header)])
    (printf "HTTP/1.1 ~A ~A~%" status reason)
    (display-header header)
    (display body)))

(define *action-table* (alist->hash-table '((get) (post) (head) (put) (delete))))

;;; action setter
(define (action-set! method path-irx proc)
  (let ([table (hash-table-ref *action-table* method)])
    (hash-table-set! *action-table*
		     method
		     (append table (list (cons path-irx proc))))))

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
			(action m request)
			(inner (cdr table))))]))
    (inner (hash-table-ref *action-table* method))))

(*server-proc* action-dispacher)

(define-syntax @
  (ir-macro-transformer
   (lambda (expr inject compare)
     (match expr
       [(_ method path-irx thunk)
	(let ([match-data (inject 'matched)]
	      [request    (inject 'request)])
	  `(action-set! ,method ,path-irx
			(lambda (,match-data ,request)
			  (send-response ,thunk))))]
       [(_ method path-irx thunk '#:status status ...)
	]))))


(@ 'get "/hello"
	(status 200)
	(reason "FOO")
	(header '((a . 200)))
	(lambda ()
	  (display "Hello")
	  (display (request-header request))))

;; (@ 'get "/hello/"
;; 	(lambda (m rq)
;; 	  (send-response
;; 	   (lambda () (display "HELLO")))))

;; (@ 'get "/foo"
;; 	(lambda (m rq)
;; 	  (send-response
;; 	   (lambda () (display "FOO")))))

;; (@ 'get '(: "/name/" (=> name (+ any)))
;; 	(lambda (m rq)
;; 	  (let ([name (irregex-match-substring m 'name)])
;; 	    (send-response
;; 	     (lambda () (printf "Your name: ~A" name))))))

;; (@ 'post "/hello"
;; 	 (lambda (m rq)
;; 	   (send-response
;; 	    (lambda ()
;; 	      (display "POT HELLO~%")
;; 	      (display (request-body request))))))

;; (@ 'post "/foo"
;; 	 (lambda (m rq)
;; 	   (send-response
;; 	    (lambda ()
;; 	      (display "POST FOO")))))

;; (@ 'get ".*"
;; 	(lambda (m rq)
;; 	  (send-response
;; 	   (lambda ()
;; 	     (display "404 NOT FOUND"))
;; 	   #:status 404
;; 	   #:reason "NOT FOUND")))

(*server-port* 8081)
(server-start)

