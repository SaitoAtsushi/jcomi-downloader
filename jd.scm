#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

;;; set your acount
(define *id* "set your id")
(define *pass* "set your password")

(use srfi-1)
(use srfi-11)
(use srfi-19)
(use rfc.cookie)
(use rfc.json)
(use rfc.http)
(use rfc.zlib)
(use gauche.parameter)
(use gauche.charconv)
(use gauche.collection)
(use util.queue)
(use rfc.uri)
(use gauche.parseopt)

(add-load-path "." :relative)
(use zip-archive)

(define-method ref ((obj <list>) (key <string>))
  (assoc-ref obj key))

(define (construct-request-cookie lst)
  (let1 u (map (lambda(x) (car (last-pair x)))
               (group-collection lst :key car :test string=?))
    (string-join (map (^[x] (string-append (car x) "=" (cdr x))) u) "; ")))

(define (header->cookies header)
  (filter-map
   (^[x] (and (string=? (car x) "set-cookie")
              (if-let1 m (#/^([^=]*)=([^;]*)/ (cadr x))
                (cons (m 1) (m 2))
                #f)))
   header))

(define (body->serial body)
  ((#/var __serial = \"([^\"]+)\";/ body) 1))

(define (body->app body)
  (if-let1 m (#/\/virgo\/app.js\?V=[^\"]+/ body)
    (m 0)
    (error "You are not premium member")))

(define session-id (make-parameter #f))

(define (login id password)
  (let1 post-data #`"_method=POST&data%5BUser%5D%5Bemail%5D=,(uri-encode-string *id*)&data%5BUser%5D%5Bpasswd%5D=,(uri-encode-string *pass*)&data%5BUser%5D%5Breferer%5D=1&data%5BUser%5D%5Bkeep%5D=0&data%5BUser%5D%5Bkeep%5D=1&x=136&y=24"
    (let*-values
        ([(status header1 body)
          (http-post "www.mangaz.com" "/login"
                     post-data
                     :Content-Type "application/x-www-form-urlencoded"
                     :secure #t
                     :redirect-handler #f)]
         [(status header2 body)
          (http-get "r18.mangaz.com" "/attention/r18/yes"
                    :Referer "http://r18.mangaz.com/attention/r18"
                    :redirect-handler #f
                    :Cookie (construct-request-cookie
                             (header->cookies header1)))])
      (header->cookies (append header2 header1)))))

(define (with-session id password thunk)
  (parameterize ((session-id (login id password)))
    (thunk)))

(define (get number :optional (try-count 0))
  (let*-values
      ([(status header body)
        (http-get "vw.mangaz.com" #`"/virgo/view/,|number|"
                  :Referer "http://vw.mangaz.com/book/detail/,|number|"
                  :redirect-handler #t
                  :Cookie (construct-request-cookie (session-id))
                  )]
       [(cookies) (append
                   (header->cookies header)
                   (session-id))]
       [(s h b)
        (let1 app (body->app body)
          (http-get "vw.mangaz.com" app
                    :cookie (construct-request-cookie cookies)))]
       [(serial) (body->serial b)]
       [(status header body)
        (http-post "vw.mangaz.com"
                   #`"/virgo/document/,|number|.json"
                   `((__ticket ,(~ cookies "virgo!__ticket"))
                     (__serial ,serial))
                   :Cookie (construct-request-cookie cookies)
                   :Referer #`"http://vw.mangaz.com/virgo/document/,|number|.json"
                   :Content-Type "application/x-www-form-urlencoded"
                   :X-Requested-With "XMLHttpRequest")])
    (if (string=? "200" status)
        (values (parse-json-string body)
                (assoc-ref cookies "_MANGAZ_"))
        (if (< try-count 1)
            (begin (session-id (login *id* *pass*))
                   (get number (+ 1 try-count)))
            (error "Retry, but failed.")))))

(define (files x)
  (filter values
          (vector-map (^[x] (assoc-ref x "file"))
                      (~ x "_doc" "Images"))))

(define (title x) (~ x "_doc" "Book" "title"))
(define (volume x) (~ x "_doc" "Book" "volume"))
(define (base x) (~ x "_doc" "Location" "base"))
(define (quality x) (~ x "_doc" "Location" "hq"))

(define (authors x)
  (string-join
   (map (cut ~ <> "name") (~ x "_doc" "Authors"))
   "×"))

(cond-expand
 [gauche.sys.threads
  (define *max-thread* (make-parameter 18))
  (use control.thread-pool)
  (define (parallel-map proc lst)
    (let1 pool (make-thread-pool (*max-thread*))
      (for-each (^x (add-job! pool (cut proc x) #t)) lst)
      (terminate-all! pool)
      (map! (cut ~ <> 'result) (queue->list (thread-pool-results pool)))
      ))]
 [else
  (define parallel-map map)])

(define (path-split url)
  (let1 m (#/^https:\/\/([^\/]+)(\/.+)$/ url)
    (values (m 1) (m 2))))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;\r\n]/ title ""))

(define (zip-encode filename lst)
  (let1 za (open-output-zip-archive filename)
    (map(^x (zip-add-entry za (first x) (second x)
                           :compression-level Z_NO_COMPRESSION))
        lst)
    (zip-close za)))

(define (download-img domain q path x cookie)
  (receive (status header body)
      (http-get domain
                (string-append path q x))
    body))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) "[options] [id] ...")
  (print "Options:\n  --listfile file   reading list from file.")
  (exit))

(define (jcomi number)
  (let*-values ([(data cookie) (get number)]
                [(fs) (files data)]
                [(b) (base data)]
                [(q) (quality data)]
                [(d) (receive(domain path) (path-split b)
                       (parallel-map
                        (^[x] (cons x (download-img domain q path x cookie)))
                        fs))])
    (zip-encode
      (sanitize
       (let* ((vol (volume data))
              (vols (if (or (eq? 'null vol) (equal? "" vol))
                        ""
                        #`" 第,|vol|巻")))
         #`"[,(authors data)] ,(title data),|vols|.zip"))
     (map (^[x] (list (car x) (cdr x))) d))))

(define file->list (cut call-with-input-file <> (pa$ port->list read-line)))

(define (main args)
  (let-args (cdr args)
      ((listfile  "l|listfile=s"  #f)
       (help  "h|help" => (cut usage (car args)))
       . targets)
    (let ((targets (if listfile (file->list listfile) targets)))
      (guard (e ((condition-has-type? e <error>)
                 (display (~ e 'message))))
        (with-session *id* *pass*
          (lambda()
            (for-each (compose jcomi) targets)))))))
