(use socket)
(define (message msg)
(let* ((unix-socket (socket af/unix sock/stream))
       (pathname "TestePP2")
       (display msg) 
       (message-to-send msg))
  (socket-connect unix-socket (unix-address pathname))
  (let ((number-of-bytes-sent (socket-send unix-socket message-to-send)))
    (printf "Number of bytes sent to the server: ~A~%" number-of-bytes-sent))
  ))

;(define close
  ;(socket-close unix-socket)
  ;)