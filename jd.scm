#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

;;; set your acount
(define *id* "set your id")
(define *pass* "set your password")

;;; set your filesystem encode
(define *fsencode*
  (cond-expand (gauche.os.windows 'Shift_JIS)
               (else 'utf8)))

(use srfi-1)
(use srfi-11)
(use srfi-19)
(use rfc.cookie)
(use rfc.json)
(use rfc.http)
(use rfc.zlib)
(use gauche.parameter)
(use gauche.charconv)
(use util.queue)
(use rfc.uri)

(add-load-path "." :relative)
(use zip-archive)

(define-method ref ((obj <list>) (key <string>))
  (assoc-ref obj key))

(define (construct-request-cookie lst)
  (string-join (map (^[x] (string-append (car x) "=" (cdr x))) lst) "; "))

(define (header->cookies header)
  (filter-map
   (^[x] (and (string=? (car x) "set-cookie")
              (if-let1 m (#/^([^=]*)=([^;]*)/ (cadr x))
                (cons (m 1) (m 2))
                #f)))
   header))

(define (body->dataset body)
  ((#/<input type=\"hidden\" value=\"([^\"]+)\" name=\"__dataset\"/ body) 1))

(define (body->serial body)
  ((#/var __serial = \"([^\"]+)\";/ body) 1))

(define (body->app body)
  ((#/\/murasame\/app.js\?V=[^\"]+/ body) 0))

(define session-id (make-parameter #f))

(define (login id password)
  (let1 post-data #`"_method=POST&data%5BUser%5D%5Bid%5D=,(uri-encode-string *id*)&data%5BUser%5D%5Bpass%5D=,(uri-encode-string *pass*)&data%5BUser%5D%5Bkeep%5D=0&data%5BUser%5D%5Bkeep%5D=1"
    (let*-values
        ([(status header1 body)
          (http-post "www.j-comi.jp" "/userlogin/index/"
                     post-data
                     :Content-Type "application/x-www-form-urlencoded"
                     :secure #t
                     :redirect-handler #f)]
         [(status header2 body)
          (http-get "r18.j-comi.jp" "/attention/r18/yes"
                    :Referer "http://r18.j-comi.jp/attention/r18vw"
                    :redirect-handler #f
                    :Cookie (construct-request-cookie
                             (header->cookies header1)))])
      (header->cookies (append header2 header1)))))

(define (with-session id password thunk)
  (parameterize ((session-id (login id password)))
    (thunk)))

(define (get number)
  (let*-values
      ([(status header body)
        (http-get "vw.j-comi.jp" #`"/murasame/view/,|number|/p:1"
                  :Referer "http://r18.j-comi.jp/attention/r18vw"
                  :redirect-handler #t
                  :Cookie (construct-request-cookie (session-id))
                  )]
       [(cookies) (append
                   (header->cookies header)
                   (session-id))]
       [(dataset) (body->dataset body)]
       [(s h b)
        (http-get "vw.j-comi.jp" (body->app body)
                  :cookie (construct-request-cookie cookies))]
       [(serial) (body->serial b)]
       [(status header body)
        (http-post "vw.j-comi.jp"
                   #`"/murasame/pages/,|number|"
                   `((__ticket ,(~ cookies "murasame!__ticket"))
                     (__dataset ,dataset)
                     (__serial ,serial))
                   :Cookie (construct-request-cookie cookies)
                   :Referer #`"http://vw.j-comi.jp/murasame/view/,|number|/p:1"
                   :Content-Type "application/x-www-form-urlencoded"
                   :X-Requested-With "XMLHttpRequest")])
    (values (parse-json-string body)
            (assoc-ref cookies "_J-COMIC_"))))

(define (files x)
  (filter-map (^[x] (assoc-ref x "file"))
              (~ x "_doc" "Pages")))

(define (title x) (~ x "_doc" "Book" "title"))
(define (volume x) (~ x "_doc" "Book" "volume"))
(define (base x) (~ x "_doc" "Location" "base"))

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
  (let1 m (#/^http:\/\/([^\/]+)(\/.+)$/ url)
    (values (m 1) (m 2))))

(define (fsencode str)
  (ces-convert str (gauche-character-encoding) *fsencode*))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;\r\n]/ title ""))

(define (zip-encode filename lst)
  (let1 za (open-output-zip-archive filename)
    (map(^x (zip-add-entry za (first x) (second x)
                           :compression-level Z_NO_COMPRESSION))
        lst)
    (zip-close za)))

(define (download-img domain path x cookie)
  (receive (status header body)
      (http-get domain
                (string-append path "st/" x)
                :Cookie #`"_J-COMIC_=,|cookie|")
    body))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " id ...")
  (exit))

(define (jcomi number)
  (let*-values ([(data cookie) (get number)]
                [(fs) (files data)]
                [(b) (base data)]
                [(d) (receive(domain path) (path-split b)
                       (parallel-map
                        (^[x] (cons x (download-img domain path x cookie)))
                        fs))])
    (zip-encode
     (fsencode
      (sanitize
       (let* ((vol (volume data))
              (vols (if (eq? 'null vol) "" #`" 第,|vol|巻")))
         #`"[,(authors data)] ,(title data),|vols|.zip")))
     (map (^[x] (list (car x) (cdr x))) d))))

(define (main args)
  (guard (e ((condition-has-type? e <error>)
             (display (~ e 'message))))
    (when (> 2 (length args)) (usage (car args)))
    (with-session *id* *pass*
      (lambda()
        (for-each (compose jcomi) (cdr args))))))
