(use srfi-18)

(define (chat num-porta)
	(define recebedor (tcp-listen num-porta 4 #t)) ;;cria o recebedor de mensagens
	(define main-thread (make-thread)) ;;cria a thread principal
	(parameterize ([current-thread main-thread])) 


	)