;(use srfi-18)
(define (chat num-porta)
    (define main-cust (make-custodian))

    (parameterize ([current-custodian main-cust])
    (define recebedor (tcp-listen num-porta 5 #t)) ;;cria o recebedor de mensagens
   ;; cria as listas que vao armazenar usuarios
   ;; e mensagens enviadas e recebidas
    (define-values (contador listaEntrada listaSaida listaUsuarios listaMensagens listaPrincipal) (values 0 null null  null null null))
      
      
    (define (loop)
      (define-values (novoIn novoOut) (aceitaETrata recebedor))
      (cond [(tcp-port? novoIn)
         (display "Ola novo usuario.\n" novoOut)
         (newline novoOut)
         (flush-output novoOut)
         (set! listaEntrada (cons novoIn listaEntrada))
         (set! listaSaida (cons novoOut listaSaida))
         (set! contador (+ contador 1))
         (set! listaUsuarios (cons contador listaUsuarios))
        ]
        )

      (set! listaPrincipal (trata-mensagens listaUsuarios listaEntrada listaSaida null))
      
      (cond [(null? listaPrincipal) 
        (set! listaUsuarios null)
        (set! listaEntrada null)
        (set! listaSaida null)
        (set! listaMensagens null)
          ]
            [(not (list? (car listaPrincipal)));se for o unico elemento da lista
               (set! listaUsuarios (list (car listaPrincipal)))
               (set! listaEntrada (list (cadr listaPrincipal)))
               (set! listaSaida (list (caddr listaPrincipal)))
               (if (not(null? (cadddr listaPrincipal)))                  
               (set! listaMensagens (list (cadddr listaPrincipal)))
               (set! listaMensagens null))
               ]
            [else  (set! listaUsuarios (car listaPrincipal))
               (set! listaEntrada (cadr listaPrincipal))
               (set! listaSaida (caddr listaPrincipal))
               (if (not(null? (cadddr listaPrincipal)))                  
               (set! listaMensagens (list (cadddr listaPrincipal)))
               (set! listaMensagens null))
               ])
      

;envia mensagens pra todos
      (enviaParaTodos listaMensagens listaSaida)
      

      (loop))

    (thread loop)

  (lambda ()
;fechar todos as threads e conexões
      (custodian-shutdown-all main-cust))))


(define (checaMensagem mensagem currentInPort currentOutPort ) 
  (cond [(string=? mensagem "sair") 
    ;;avisar a thread que a porta fechará
    (display "Tchau!" currentOutPort)
    (newline currentOutPort)
    (flush-output currentOutPort)
    ;;termina a conexao
    (close-input-port currentInPort)
    (close-output-port currentOutPort)
    (list "sair" 0)
    ]

    [(and (< 4 (string-length mensagem)) (string=? (substring mensagem 0 4) "nick") )
    (display "Nick mudou" currentOutPort)
    (newline currentOutPort)
    (flush-output currentOutPort)
     
    (list "nick" (substring mensagem 5))
    ]

    ;[(string=? mensagem "lista") 
    ;(display "Lista de usuarios:" currentOutPort)
    ;(newline currentOutPort)
    ;(display (listarUsuarios listaUsuarios) currentOutPort)
    ;(newline currentOutPort)
    ;(flush-output currentOutPort)
    ;null
    ;]

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
     ]
    )
         
  
  )

(define (aceitaETrata recebedor)

  (define-values (entra sai) (values -1 -1))
  (cond [(tcp-accept-ready? recebedor) 
     (set!-values (entra sai) (tcp-accept recebedor))]
   )
  (values entra sai))

(define (trata-mensagens listaUsuarios inClientes outClientes listaMensagens)
  
  (define-values (nick portaEntrada portaSaida) (values null null null))
  (define mensagem null)
  (define-values (mensagemEspecial parametro) (values null null))
  (define-values (tmpNick tmpEntrada tmpSaida tmpMensagens) (values null null null null))
  (define tmpListaPrincipal null)
  (define tmp null)
  
  (cond [(not (null? listaUsuarios)) 
     (set!-values (nick portaEntrada portaSaida) (values (car listaUsuarios) (car inClientes) (car outClientes)))
     ;;le a mensagem
     (cond [(char-ready? portaEntrada)
         (set! mensagem (read-line portaEntrada))
         (set! tmp  (checaMensagem mensagem portaEntrada portaSaida))
         
         (cond [(not(null? tmp))
            (set! mensagemEspecial (car tmp))
            (set! parametro (cadr tmp))
            ])
         ])
            ;sair do programa                      
           (cond [(and (not(null? mensagemEspecial)) (string=? mensagemEspecial "sair")) 
               (close-input-port portaEntrada)
               (close-output-port portaSaida) 
               ;;exclui a porta
               (set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
               (cond [ (null? tmpListaPrincipal) null]
                  [else
                   (set! tmpNick (car tmpListaPrincipal))
                   (set! tmpEntrada (cadr tmpListaPrincipal))
                   (set! tmpSaida (caddr tmpListaPrincipal))
                   (set! tmpMensagens(cadddr tmpListaPrincipal))
                   (list tmpNick tmpEntrada tmpSaida tmpMensagens)
                   ])
                ]
                ;;troca nick
           [(and (not(null? mensagemEspecial)) (string=? mensagemEspecial "nick")) 
               (set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
               
              (cond [(null? tmpListaPrincipal) (list parametro portaEntrada portaSaida listaMensagens)]
                [else
                 (set! tmpNick (geradorListaCorreta parametro (car tmpListaPrincipal)))
                 (set! tmpEntrada (geradorListaCorreta portaEntrada (cadr tmpListaPrincipal)))
                 (set! tmpSaida (geradorListaCorreta portaSaida (caddr tmpListaPrincipal)))
                 (set! tmpMensagens(cadddr tmpListaPrincipal))
                 (list tmpNick tmpEntrada tmpSaida tmpMensagens)
               ])
               ]
            ;; lista usuarios logados
               ;[(and (not(null? mensagemEspecial)) (string=? mensagemEspecial "lista")) 
               ;(set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
              ; 
              ;(cond [(null? tmpListaPrincipal) (list parametro portaEntrada portaSaida listaMensagens)]
              ;  [else
              ;   (set! tmpNick (geradorListaCorreta parametro (car tmpListaPrincipal)))
              ;   (set! tmpEntrada (geradorListaCorreta portaEntrada (cadr tmpListaPrincipal)))
              ;   (set! tmpSaida (geradorListaCorreta portaSaida (caddr tmpListaPrincipal)))
              ;   (set! tmpMensagens(cadddr tmpListaPrincipal))
              ;   (list tmpNick tmpEntrada tmpSaida tmpMensagens)
              ; ])
              ; ]



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
                  (set! tmpNick (geradorListaCorreta nick (car tmpListaPrincipal)))
                  (set! tmpEntrada (geradorListaCorreta portaEntrada (cadr tmpListaPrincipal)))
                  (set! tmpSaida (geradorListaCorreta portaSaida (caddr tmpListaPrincipal)))
                  (set! tmpMensagens (geradorListaCorreta mensagem (cadddr tmpListaPrincipal)))
                  
                  (list tmpNick tmpEntrada tmpSaida tmpMensagens)                                          
                      ])
                         ]
           [else (set! tmpListaPrincipal (trata-mensagens (cdr listaUsuarios) (cdr inClientes) (cdr outClientes) listaMensagens))
                 (cond[ (null? tmpListaPrincipal) (list nick portaEntrada portaSaida listaMensagens)]
                    [else
                     
                     (set! tmpNick (geradorListaCorreta nick (car tmpListaPrincipal)))
                     (set! tmpEntrada (geradorListaCorreta portaEntrada (cadr tmpListaPrincipal)))
                     (set! tmpSaida (geradorListaCorreta portaSaida (caddr tmpListaPrincipal)))
                     (set! tmpMensagens(cadddr tmpListaPrincipal))
                     (list tmpNick tmpEntrada tmpSaida tmpMensagens)
                     ])])]
        [else null]
        ) 
  )




(define (enviaParaTodos listaMensagens outClientes)
  (cond [(not (null? outClientes))
     (enviaTodasMensagens listaMensagens (car outClientes))
    (enviaParaTodos listaMensagens (cdr outClientes))
    ]
  )
 )
  

  ;;funcoes uteis
(define (removeChar aString) (substring aString 0 (- (string-length aString) 1)) )
(define (cleanOutput aString) (cond [(list? aString) (car aString)]
    [else aString]))
(define (geradorListaCorreta a b) (cond [(list? b) 
    (cons a b)]
   [else
    (list a b)]
   )
  )

(define (listarUsuarios listaUsuarios)
 ; (define listaRetorno '())
  (let listarUsuariosTemp ((listaRetorno listaUsuarios))
    (if (null? listaRetorno)
      '()
      (cons (car listaRetorno) (listarUsuariosTemp (cdr listaRetorno))))))
