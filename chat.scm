(use srfi-18)

(define (chat num-porta)
	(define recebedor (tcp-listen num-porta 4 #t)) ;;cria o recebedor de mensagens
	(define main-thread make-thread) ;;cria a thread principal
	;;(parameterize ([current-thread main-thread])) 
	(define listaMensagens null) ;; cria as listas que vao armazenar usuarios
	(define listaPrincipal null) ;; e mensagens enviadas e recebidas
	(define listaEntrada null)
	(define listaUsuarios null)
	(define listaSaida null)
	(define contador 0)
	

	(define (loop)

		(define novoIn accept-and-handle)
		(define novoOut recebedor)

		(cond [(tcp-port? novoIn)
			(display "olá novo usuario " novoOut)
			(newline novoOut)
			(flush-output novoOut)

			(set! listaEntrada (cons novoIn listaEntrada))
			(set! listaSaida (cons novoOut listaSaida))

			(set! contador (+ contador 1))
			(set! listaUsuarios (cons contador listaUsuarios))
			])

		(set! listaPrincipal (handle-messages listaUsuarios listaEntrada listaSaida null))

		(cond [(null? listaPrincipal) (set! listaUsuarios null)
									  (set! listaUsuarios null)
									  (set! listaUsuarios null)
									  (set! listaUsuarios null)])
				[(not (list? (car listaPrincipal)))
						(set! listaUsuarios (list (car listaPrincipal)))
						(set! listaEntrada (list (cadr listaPrincipal)))
						(set! listaSaida (list (caddr listaPrincipal)))

						(if (not (null? (cadddr listaPrincipal))))
						(set! listaMensagens (list (cadddr listaPrincipal)))
						(set! listaMensagens null))]
				[else (set! listaUsuarios (car listaPrincipal))
					  (set! listaEntrada (cadr listaPrincipal))
					  (set! listaSaida (caddr listaPrincipal))

					  (if (not(null? (cadddr listaPrincipal)))
					  (set! listaMensagens (list (cadddr listaPrincipal)))
					  (set! listaMensagens null))
						])
	
	)
