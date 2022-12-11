(define-module (github search-repositories)
  #:use-module (json)
  #:use-module (gnutls)
  #:use-module ((web client) #:select (http-request))
  #:use-module ((web response) #:select (response-headers))
  #:use-module ((ice-9 receive) #:select (receive))
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((srfi srfi-1) #:select (find))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((rnrs) :version (6) :select (utf8->string))

  #:export (search-repositories))

;; some useful modules I've used along the way.

;; (use-modules (web request))
;; (use-modules (web http))
;; (use-modules (scheme base))
;; (use-modules (ice-9 iconv))
;; (use-modules (ice-9 pretty-print))

(define-record-type <link>
  (make-link url rel)
  link?
  (url link-url)
  (rel link-rel))

(define-json-mapping <repository>
  make-repository
  repository?
  json->repository <=> repository->json <=> scm->repository <=> repository->scm
  (ssh_url repository-ssh_url))

(define-json-mapping <repositories>
  make-repositories
  repositories?
  json->repositories
  (items    repositories-items "items"
            (lambda (r) (map scm->repository (vector->list r)))
            (lambda (r) (list->vector (map repository->scm r)))))

(define (parse-links s)
  (map parse-link (string-split s #\,)))

(define (parse-link s)
  (make-link
   (extract (car (string-split s #\;)) #\< #\>)
   (extract (cadr (string-split s #\;)) #\" #\")))

(define (extract s a b)
  (let ((beginning (+ (string-index s a) 1))
        (end (string-rindex s b)))
    (substring s beginning end)))

(define (search url)
  (call-with-values
      (lambda () (github-request url))
    (lambda (response body)
      (let ((next (find-next-link-from-response response)))
        (if next
            (append (repositories-items (json->repositories (utf8->string body))) (search (link-url next)))
            (append (repositories-items (json->repositories (utf8->string body))) '()))))))

(define (find-next-link-from-response response)
  (let ((links (assoc-ref (response-headers response) 'link)))
    (if links
        (find (lambda (link) (if (equal? (link-rel link) "next") #t #f)) (parse-links links))
        #f)))

(define (github-request url pat)
  (receive (response body)
      (http-request url #:headers '((authorization . (bearer pat))))
    (values response body)))

(define (search-repositories q)
  (search (format #f "https://github.com/api/v3/search/repositories?q=~a" q)))
