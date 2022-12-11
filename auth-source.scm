(define-module (authinfo)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module ((srfi srfi-9) #:select (define-record-type))

  #:export (parse-authinfo))

(define-record-type <entry>
  (make-entry machine login password)
  entry?
  (machine entry-machine)
  (login entry-login)
  (password entry-password))

(define (parse-entry entry)
  (let ((tokens (string-split entry #\ )))
    (make-entry
     (list-ref tokens 1)
     (list-ref tokens 3)
     (list-ref tokens 5))))

(define (parse-authinfo filename)
  (call-with-input-file filename
    (lambda (port)
      (map (lambda (entry) (parse-entry entry)) (read-lines-from-port port)))))

(define (read-lines-from-port port)
  (let ((o (read-line port)))
    (if (not (eof-object? o))
        (cons o (read-lines-from-port port))
        '())))
