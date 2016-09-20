(use niwatori)
(@ 'get "/hello"
	(lambda ()
	  (display $header)))

(@ 'get "/name/(?<name>[^/]+)"
	(lambda ()
	  (display ($matched 'name))))

(@ 'post "/name/(?<name>[^/]+)"
	(lambda ()
	  (printf "~A: ~A" ($matched 'name) $body)))

(*server-port* 8081)
(server-start)
