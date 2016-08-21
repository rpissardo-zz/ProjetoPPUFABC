;(use srfi-18)

(define (chat num-porta)

  ;(define main-thread make-thread) ;;cria a thread principal
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust]) 

  (define recebedor (tcp-listen num-porta 5 #t)) ;;cria o recebedor de mensagens
  (define listaMensagens null) ;; cria as listas que vao armazenar usuarios
  (define listaPrincipal null) ;; e mensagens enviadas e recebidas
  (define listaEntrada null)
  (define listaUsuarios null)
  (define listaSaida null)
  (define contador 0)
  


    (define (loop)

     ; (define novoIn aceitaETrata)
      ;(define novoOut recebedor)
      (define-values (novoIn novoOut) (aceitaETrata recebedor))
      (cond [(tcp-port? novoIn)
        (display "ola novo usuario \n" novoOut)
        (newline novoOut)
        (flush-output novoOut)

        (set! listaEntrada (cons novoIn listaEntrada))
        (set! listaSaida (cons novoOut listaSaida))

        (set! contador (+ contador 1))
        (set! listaUsuarios (cons contador listaUsuarios))
        ])

    (set! listaPrincipal (trata-mensagens listaUsuarios listaEntrada listaSaida null))

    (cond [(null? listaPrincipal)
                    (set! listaUsuarios null)
                    (set! listaEntrada null)
                    (set! listaSaida null)
                    (set! listaMensagens null)]
        [(not (list? (car listaPrincipal))) ;se for o unico elemento da lista
            (set! listaUsuarios (list (car listaPrincipal)))
            (set! listaEntrada (list (car (cdr  listaPrincipal))))
            (set! listaSaida (list (caddr listaPrincipal)))

            (if (not (null? (cadddr listaPrincipal)))
            (set! listaMensagens (list (cadddr listaPrincipal)))
            (set! listaMensagens null))]
        [else (set! listaUsuarios (car listaPrincipal))
            (set! listaEntrada (car (cdr  listaPrincipal)))
            (set! listaSaida (caddr listaPrincipal))

            (if (not(null? (cadddr listaPrincipal)))
            (set! listaMensagens (list (cadddr listaPrincipal)))
            (set! listaMensagens null))
            ])
      ;envia mensagens pra todos
      (enviaMensagens listaMensagens listaSaida)
      

      (loop))

    (thread loop)

  (lambda ()
;fechar todos as threads e conexões
      (custodian-shutdown-all main-cust))))

 ;;comandos nas mensagens
(define (checaMensagem mensagem currentInPort currentOutPort) 
        (cond [(string=? mensagem "sair") 
      ;;avisar a thread que a porta fechará
      (display "Saindo.." currentOutPort)
      (newline currentOutPort)
      (flush-output currentOutPort)
      ;;terminate de connection
      (close-input-port currentInPort)
      (close-output-port currentOutPort)
      (list "sair" 0)
      ]
      [(and (< 4 (string-length mensagem)) (string=? (substring mensagem 0 4) "nick") )
      (display "Nick mudado" currentOutPort)
      (newline currentOutPort)
      (flush-output currentOutPort)

      (list "nick" (substring mensagem 5))
      ]
      [else null]
    )                   
)

(define (aceitaETrata recebedor)
    (define entra -1)
    (define sai -1)

    (cond [(tcp-accept-ready? recebedor) 
    (set!-values (entra sai) (tcp-accept recebedor))]
    )
    (values entra sai))
  
  
  ;;essa funcao recebe parametros especiais e as listas de usuarios, entrada, saida e mensagens


(define (trata-mensagens listaUsuarios inClientes outClientes listaMensagens)
    (define nick null)
    (define portaEntrada null)
    (define portaSaida null)
    (define mensagem null)

    (define tmpNick null)
    (define tmpEntrada null)
    (define tmpSaida null)
    (define tmpMensagens null)

    (define mensagemEspecial null)
    (define parametro null)

    (define tmpListaPrincipal null)
    (define tmp null)
    
    (cond [(not (null? listaUsuarios)) 
            (set!-values (nick portaEntrada portaSaida) (values (car listaUsuarios) (car inClientes) (car outClientes)))
             ;;le a mensagem
            (cond [(char-ready? portaEntrada)
                (set! mensagem (read-line portaEntrada))
                 ;;
                (set! tmp  (checaMensagem mensagem portaEntrada portaSaida))
                 
                (cond [(not(null? tmp))
                    (set! mensagemEspecial (car tmp))
                    (set! parametro (car (cdr  tmp)))
                    ])
                ])
                                    
             ;;retorna o que resta da lista
            (cond [(and (not(null? mensagemEspecial)) (string=? mensagemEspecial "sair")) 
                (close-input-port portaEntrada)
                (close-output-port portaSaida) 
                ;;deleta a porta que estava aberta
                (set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
                (cond [ (null? tmpListaPrincipal) null]
                    [else
                    ;;se nao for o ultimo elemento
                    (set! tmpNick (car tmpListaPrincipal))
                    (set! tmpEntrada (car (cdr  tmpListaPrincipal)))
                    (set! tmpSaida (caddr tmpListaPrincipal))
                    (set! tmpMensagens(cadddr tmpListaPrincipal))
                    ;;retornar lista ao inves de elementos soltos pois facilita a leitura
                    (list tmpNick tmpEntrada tmpSaida tmpMensagens)
                    ])
                ]
            [(and (not(null? mensagemEspecial)) (string=? mensagemEspecial "nick")) 
                (set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
                
                (cond [(null? tmpListaPrincipal) (list parametro portaEntrada portaSaida listaMensagens)]
                    [else
                    (set! tmpNick (correctListGenerator parametro (car tmpListaPrincipal)))
                    (set! tmpEntrada (correctListGenerator portaEntrada (car (cdr  tmpListaPrincipal))))
                    (set! tmpSaida (correctListGenerator portaSaida (caddr tmpListaPrincipal)))
                    (set! tmpMensagens(cadddr tmpListaPrincipal))
                    (list tmpNick tmpEntrada tmpSaida tmpMensagens)
                ])
                 ]
            [(not(null? mensagem))
                (set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
                (set! mensagem (string-append ": " mensagem))
                (if (not (string? nick)) 
                    (set! mensagem (string-append (number->string nick) mensagem))
                    (set! mensagem (string-append nick mensagem))
                    )
                (cond [(null? tmpListaPrincipal) 
                    (list nick portaEntrada portaSaida mensagem)
                    ]
                    [else
                    (set! tmpNick (correctListGenerator nick (car tmpListaPrincipal)))
                    (set! tmpEntrada (correctListGenerator portaEntrada (car (cdr  tmpListaPrincipal))))
                    (set! tmpSaida (correctListGenerator portaSaida (caddr tmpListaPrincipal)))
                    (set! tmpMensagens (correctListGenerator mensagem (cadddr tmpListaPrincipal)))
                    (list tmpNick tmpEntrada tmpSaida tmpMensagens)                                          
                    ])
                 ]
            [else (set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
                (cond[ (null? tmpListaPrincipal) (list nick portaEntrada portaSaida listaMensagens)]
                    [else
                     
                    (set! tmpNick (correctListGenerator nick (car tmpListaPrincipal)))
                    (set! tmpEntrada (correctListGenerator portaEntrada (car (cdr  tmpListaPrincipal))))
                    (set! tmpSaida (correctListGenerator portaSaida (caddr tmpListaPrincipal)))
                    (set! tmpMensagens(cadddr tmpListaPrincipal))
                     
                    (list tmpNick tmpEntrada tmpSaida tmpMensagens)
                    ])
                ])]
          [else null]
          )
    )
  
  
(define (enviaTodasMensagens listaMensagens portaSaida)
    (define tmpString null)
    (cond [(not(null? listaMensagens))
           
    (set! tmpString (car listaMensagens))
    (set! tmpString (cleanOutput tmpString))
    (display tmpString portaSaida)
    (newline portaSaida)
    (flush-output portaSaida)
    (enviaTodasMensagens (cdr listaMensagens) portaSaida)
    ]))

(define (enviaMensagens listaMensagens outClientes)
    (cond [(not (null? outClientes))
    (enviaTodasMensagens listaMensagens (car outClientes))
    (enviaMensagens listaMensagens (cdr outClientes))
    ]))
  


  ;;funcoes uteis
  ;;
  
(define (removeChar aString) (substring aString 0 (- (string-length aString) 1)) )

(define (cleanOutput aString) (cond [(list? aString) (car aString)]
          [else aString]))
(define (correctListGenerator a b) (cond [(list? b) 
            (cons a b)]
           [else
            (list a b)]
           )
)
(define (start) (chat 8090))
