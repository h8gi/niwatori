(use tcp6 uri-common irregex posix defstruct)
(defstruct client fileno in out)
(define *client-table*  (make-hash-table))
(define *server-proc*
  (make-parameter
   (lambda (request)
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
    (make-client #:fileno (port->fileno i)
		 #:in i
		 #:out o)))
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

;;; request ==================================================
(defstruct request
  method uri http-version header body)

(define (read-request #!optional (in (current-input-port)))
  (with-input-from-port in
    (lambda ()
      (read-request-body (read-request-header)))))

;;; header ------------------------------------------------
(define (parse-request-line line)
  (let ([m (irregex-match (irregex '(: (=> method (+ (~ space)))
				       (+ space)
				       (=> uri (+ (~ space)))
				       (+ space)
				       (=> http-version (+ (~ space)))) 'i)
			  (if (eof-object? line) "" line))])
    (if m
	(let ([method (irregex-match-substring m 'method)]
	      [uri    (irregex-match-substring m 'uri)]
	      [http-version (irregex-match-substring m 'http-version)])
	  ;; (list (cons 'http-version http-version)
	  ;; 	(cons 'uri uri)
	  ;; 	(cons 'method (string->symbol (string-downcase method))))
	  (values ((compose string->symbol string-downcase) method) uri http-version))
	(values #f #f #f))))

(define (parse-request-header-line line)
  (let ([m (irregex-match (irregex "([^:]+): *(.+)" 'i) line)])
    (if m (cons (string->symbol (string-downcase (irregex-match-substring m 1)))
                (string-trim-both (irregex-match-substring m 2)))
        #f)))


(define (read-request-header #!optional (in (current-input-port)))
  (receive (method uri http-version) (parse-request-line (read-line in))
    (let ([request (make-request #:method method
				 #:uri uri
				 #:http-version http-version)])
      (let loop ([line (read-line in)]
		 [acc  '()])
	(if (or (eof-object? line) (irregex-match '(* space) line))
	    (update-request request #:header (reverse! acc))
	    (let ([m (parse-request-header-line line)])
	      (loop (read-line in)
		    (if m (cons m acc) acc))))))))
;;; body------------------------------------------------
(define (read-request-body request #!optional (in (current-input-port)))
  (let ([header-alst (request-header request)])
    (update-request request
		    #:body
		    (cond [(alist-ref 'content-length header-alst)
			   =>
			   (lambda (content-length-str)
			     (read-string (or (string->number content-length-str) 0) in))]
			  [else ""]))))


;;; response ==================================================
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

;;; (proc request)
(define (serve-client client proc)
  (let* ([request	(read-request (client-in client))])
    (pp (request-header request))
    (with-output-to-port (client-out client)
      (lambda ()
	(proc request))))
  (close-client client))
