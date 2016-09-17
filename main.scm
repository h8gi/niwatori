;;; main.scm    source of niwatori.scm
(include "server.scm")

(define *action-table* (alist->hash-table '((get) (put))))

(define (make-action-setter method)
  (lambda (irx proc)
    (let ([table (hash-table-ref *action-table* method)])
      (hash-table-set! *action-table*
		       method
		       (append table (list (cons irx proc)))))))

;;; first in, first out
(define get (make-action-setter 'get))
(define post (make-action-setter 'post))

(define (action-loop method uri)
  (define (inner table)
    (cond [(null? table) (display "NO")]
	  [else (let* ([pair (car table)]
		       [irx  (car pair)]
		       [action (cdr pair)]
		       [m (irregex-match irx uri)])
		  (if m (action m) (inner (cdr table))))]))
  (inner (hash-table-ref *action-table* method)))

(*server-proc*
 (lambda (client header request-body)
   (let ([method  (alist-ref 'method header)]
	 [uri     (alist-ref 'uri header)])     
     (action-loop method uri))))


(get "/hello"
     (lambda (m)
       (display "HELLO")))

(get "/foo"
     (lambda (m)
       (display "FOO")))

(get '(: "/name/" (=> name (+ any)))
     (lambda (m)
       (display (irregex-match-substring m 'name))))



(*server-port* 8081)
(server-start)

