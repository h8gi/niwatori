(use tcp6 uri-common irregex posix)
(define-record client fileno in out)
(define *client-table*  (make-hash-table))
(define *server-proc*
  (make-parameter
   (lambda (client header request-body)
     (display "Hello World!"))))
(define *server-port*
  (make-parameter 8080))

(define (server-start)
  (define listener (tcp-listen (*server-port*)))
  (define listener-fileno (tcp-listener-fileno listener))
  (define (fileno-list)
    (cons* listener-fileno
	   (port->fileno (current-input-port))
	   (hash-table-keys *client-table*)))
  (let loop ()
    (printf "fileno: ~A~%" (fileno-list))
    (display "server csi> ")
    (flush-output)
    (receive (readfdlist writefdlist) (file-select (fileno-list) '())
      (cond [(member listener-fileno readfdlist) ; new client
	     (let ([client (add-new-client! listener)])
	       (display "New client: ")
	       (write-line (client-info client)))]
	    ;; serverside debug repl
	    [(member (port->fileno (current-input-port)) readfdlist)
	     (let ([expr (read)])
	       (handle-exceptions exn
		   (begin (display expr) (newline))
		 (display (eval expr))
		 (newline)))]
	    ;; main serve
	    [readfdlist
	     (for-each (lambda (fileno)
			 (let ([client (hash-table-ref/default *client-table* fileno #f)])
			   (when client
			     ;; なんか駄目だったら閉じろ
			     (handle-exceptions exn
				 (begin (display "ERROR: SERVER CLIENT\n")
					(pp (condition->list exn))
					(close-client client))
			       (serve-client client (*server-proc*))))))
		       readfdlist)]))
    (loop)))

;;; clinet --------------------------------------------------
;;; 新規接続
(define (new-client listener)
  (receive (i o) (tcp-accept listener)
    (make-client (port->fileno i) i o)))
;;; clientをテーブルに追加
(define (add-client! client)
  (hash-table-set! *client-table* (client-fileno client) client))
;;; 新規接続まとめ
(define (add-new-client! listener)
  (let ([client (new-client listener)])
    (add-client! client)
    client))

(define (client-info client)
  (receive (server-ip client-ip) (tcp-addresses (client-in client))
    (receive (server-port client-port) (tcp-port-numbers (client-in client))
      (sprintf "client ~A:~A, server ~A:~A" client-ip client-port server-ip server-port))))

(define (close-client client)
  (close-input-port (client-in client))
  (close-output-port (client-out client))
  (hash-table-delete! *client-table* (client-fileno client)))


;;; header --------------------------------------------------
(define (parse-request-line line)
  (let ([m (irregex-match (irregex '(: (=> method (+ (~ space)))
				       (+ space)
				       (=> uri (+ (~ space)))
				       (+ space)
				       (=> http-version (+ (~ space)))) 'i)
			  line)])
    (if m
	(let ([method (irregex-match-substring m 'method)]
	      [uri    (irregex-match-substring m 'uri)]
	      [http-version (irregex-match-substring m 'http-version)])
	  (list (cons 'http-version http-version)
		(cons 'uri uri)
		(cons 'method (string->symbol (string-downcase method)))))
	'())))

(define (parse-request-header-line line)
  (let ([m (irregex-match (irregex "([^:]+): *(.+)" 'i) line)])
    (if m (cons (string->symbol (string-downcase (irregex-match-substring m 1)))
                (string-trim-both (irregex-match-substring m 2)))
        #f)))

(define (read-request-header #!optional (in (current-input-port)))
  (let ([request-line (parse-request-line (->string (read-line in)))])
    (let loop ([line (read-line in)]
	       [acc request-line])
      (if (or (eof-object? line) (irregex-match '(* space) line))
	  (reverse! acc)
	  (let ([m (parse-request-header-line line)])
	    (loop (read-line in)
		  (if m (cons m acc) acc)))))))

(define (display-header header-alst #!optional (out (current-output-port)))
  (with-output-to-port out
    (lambda ()
      (for-each (lambda (lst)
                  (printf "~A: " (car lst))
                  (if (list? (cdr lst))
                      (for-each (cut printf "~A " <>) (cdr lst))
                      (printf "~A" (cdr lst)))
                  (newline))
                header-alst)
      (newline))))
;;; body--------------------------------------------------
(define (read-request-body header-alist #!optional (in (current-input-port)))
  (cond [(alist-ref 'content-length header-alist)
	 =>
	 (lambda (content-length-str)
	   (read-string (or (string->number content-length-str) 0) in))]
	[else ""]))

;;; (proc client header request-body)
(define (serve-client client proc)
  (let* ([header	(read-request-header (client-in client))]
	 [request-body  (read-request-body header (client-in client))]
	 [response-body (with-output-to-string
			    (lambda ()
			      (proc client header request-body)))]
	 [len (if (string? response-body) (string-length response-body))])
    (pp header)
    (with-output-to-port (client-out client)
      (lambda ()
	(write-line "HTTP/1.1 200 OK")
	(display-header `((content-type "text/html")
			  (content-length ,len)))
	(display response-body))))
  (close-client client))
