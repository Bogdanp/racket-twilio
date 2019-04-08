#lang racket/base

(require json
         net/base64
         net/http-client
         net/uri-codec
         racket/contract
         racket/format
         racket/match
         racket/string)

(provide
 (struct-out twilio)
 twilio-host
 twilio-api-root
 twilio-port
 twilio-ssl?
 twilio-send-message)

(define-logger twilio)

(struct twilio (account-sid auth-token))
(struct exn:fail:twilio exn:fail (code))

(define/contract twilio-host
  (parameter/c non-empty-string?)
  (make-parameter "api.twilio.com"))

(define/contract twilio-api-root
  (parameter/c non-empty-string?)
  (make-parameter "/2010-04-01"))

(define/contract twilio-port
  (parameter/c (integer-in 0 65534))
  (make-parameter 443))

(define/contract twilio-ssl?
  (parameter/c boolean?)
  (make-parameter #t))

(define/contract (twilio-send-message client
                                      #:to to
                                      #:from from
                                      #:body body)
  (->* (twilio?
        #:to non-empty-string?
        #:from non-empty-string?
        #:body non-empty-string?)
       (hash/c symbol? any/c))

  (define authorization (make-authorization-header client))
  (define path (make-api-path client "Messages.json"))
  (define headers
    (list authorization
          "Accept: application/json"
          "Content-type: application/x-www-form-urlencoded"
          (~a "User-agent: " USER-AGENT)))

  (define data
    (alist->form-urlencoded `((To . ,to)
                              (From . ,from)
                              (Body . ,body))))

  (log-twilio-debug "sending http request to ~v with headers ~v and data ~v" path headers data)
  (call-with-twilio-connection client
    (lambda (conn)
      (define-values (status-line response-headers in)
        (http-conn-sendrecv!
         conn path
         #:method "POST"
         #:headers headers
         #:data data
         #:close? #t))

      (define-values (status-code status-message)
        (parse-status-line status-line))

      (log-twilio-debug "received response status ~v" status-code)

      (define response (read-json in))
      (unless (= status-code 200)
        (raise (exn:fail:twilio
                (hash-ref response 'message)
                (current-continuation-marks)
                (hash-ref response 'code))))

      response)))


;; Private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define STATUS-LINE-REGEXP
  #px"^HTTP/... (\\d+) (.+)$")

(define USER-AGENT
  (format "Twilio Client for Racket ~a [0.1.0]" (version)))

(define (call-with-twilio-connection client f)
  (f (http-conn-open (twilio-host)
                     #:port (twilio-port)
                     #:ssl? (twilio-ssl?))))

(define (make-api-path client . path)
  (apply ~a (twilio-api-root) "/Accounts/" (twilio-account-sid client) "/" path))

(define (make-authorization-header client)
  (define credentials (~a (twilio-account-sid client) ":" (twilio-auth-token client)))
  (define payload (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 credentials) #"")))
  (~a "Authorization: Basic " payload))

(define (parse-status-line status-line)
  (match-define (list _ code-bs message-bs)
    (regexp-match STATUS-LINE-REGEXP status-line))
  (values (string->number (bytes->string/utf-8 code-bs))
          (string-downcase (bytes->string/utf-8 message-bs))))
