(use-modules (github search-repositories))

(define (clone repositories)
  (for-each
   (lambda (repository) (system (format #f "git clone ~a" (repository-ssh_url repository))))
   (repositories-items repositories)))
