;; (define-module (florilegium)
;;    #:export read-source source-dir)

(use-modules (ice-9 format)
             (ice-9 ftw)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-9)
             (srfi srfi-11))

(define source-dir "/home/adhearn/Dropbox/exobrain/florilegium/sources")

(define-record-type source
  (make-source title author quotes)
  source?
  (title source-title)
  (author source-author)
  (quotes source-quotes))

(define-record-type source-quote
  (make-quote body location tags)
  quote?
  (body quote-body)
  (location quote-location)
  (tags qoute-tags))

(define (text-file? filename)
  (string-match "\\.txt$" filename))

(define (valid-source? name)
  (and (not (string=? name "."))
       (not (string=? name ".."))
       (text-file? name)))

(define (source-filenames)
  (scandir source-dir valid-source?))

(define (parse-tags tags)
  tags)

(define (read-header port)
  (let* [(title (read-line port))
         (author (read-line port))]
    (values title author)))

;; Read empty lines from port until there are no more
(define (trim-empty-lines port)
  (let [(c (peek-char port))]
    (cond
     ((eof-object? c) '())
     ((char=? c #\newline) (read-char port) (trim-empty-lines port))
     (else '()))))

(define (read-quote port)
  (define (loop-quote port)
    (let [(ln (read-line port))]
      (if (or (eof-object? ln) (string=? ln ""))
          '()
          (cons ln (loop-quote port)))))
  (trim-empty-lines port)
  (let [(quote-text (loop-quote port))]
    (let* [(rev (reverse quote-text))
           (tags (car rev))
           (location (cadr rev))
           (body (string-join (reverse (cddr rev)) "\n"))]
      (make-quote body location (parse-tags tags)))))

(define (read-source port)
  (define (loop-quotes)
    (let [(c (peek-char port))]
      (if (eof-object? c)
          '()
          (cons (read-quote port) (loop-quotes)))))
  (let-values [((title author) (read-header port))]
    (let [(quotes (loop-quotes))]
      (make-source title author quotes))))

(define (build-source-db)
  (let [(filenames (map (lambda (f)
                          (string-append source-dir "/" f))
                        (source-filenames)))]))
