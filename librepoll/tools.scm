(define-module (librepoll tools)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module ((ice-9 format) #:prefix icef:)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-28)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs sorting)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (sxml simple)
  #:export (substitute
            sxml->xml-string
            decode-post-uri-body
            percent
            random-number))

(define (substitute str old new)
  "Replace OLD with NEW in STR."
  (let ((vstr (string-match old str)))
    (if (not (equal? vstr #f))
        (match (vector-ref vstr 1)
          ((s . e)
           (substitute (string-replace str new s e)
                       old new))
          (x
           (throw 'substitute-error '(str old new))))
        str)))

(define (sxml->xml-string sxml)
  (call-with-output-string
    (lambda (port)
      (sxml->xml sxml port))))

(define (decode-post-uri-body body)
  "Decode and split BODY in uri format. Return list of alist."
  (map (lambda (l)
         (car (acons (list-ref l 0) (uri-decode (list-ref l 1)) '())))
       (map (lambda (s)
              (string-split s #\=))
            (string-split (utf8->string body) #\&))))

(define (percent num total)
  (cond
   ((zero? total) 0)
   (else
    (let ((cur (* (/ num total) 100.0)))
      (/ (round (* cur 100)) 100.0)))))

(define (random-number)
  (random (+ (vector-ref (times) 0)
             (vector-ref (times) 1))))
