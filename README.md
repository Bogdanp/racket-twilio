# racket-twilio

A tiny Twilio client for racket.

## Example

```racket
(require twilio)

(define client
  (twilio "ACCOUNT_SID_HERE" "AUTH_TOKEN_HERE"))

(twilio-send-message client
                     #:to "+15551231234"
                     #:from "+15551231234"
                     #:body "Hello!")
```

## License

    racket-twilio is licensed under the 3-Clause BSD license.
