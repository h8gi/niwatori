;;; main.scm    source of niwatori.scm
(include "server.scm")

(*server-proc*
 (lambda (client header request-body)
   (let ([method  (alist-ref 'method header)]
	 [uri     (alist-ref 'uri header)])     
     (display "<!doctype html>\n")
     (display "<html lang=\"ja\">\n")
     (printf "<p> method: ~A</p>\n" method)
     (printf "<p> uri: ~A</p>\n" uri)
     (display "</html>"))))

(*server-port* 8081)

(server-start)

