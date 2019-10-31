;;; LibrePoll -- Free opinion poll service.
;;;
;;; Copyright (C) 2019 Pierre-Antoine Rouby <contact@parouby.fr>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (librepoll render)
  #:use-module (librepoll tools)
  #:use-module (librepoll sql-request)
  #:use-module (librepoll chart)
  #:use-module (rnrs sorting)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 format) #:prefix icef:)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-69)
  #:use-module (sxml simple)
  #:use-module (commonmark)
  #:use-module (web request)
  #:use-module (web server)
  #:use-module (web uri)
  #:export (sxml->html
            poll-info
            render-page-index
            render-page-create-poll-get
            render-page-add-option
            render-page-result
            render-page-poll))

(define (sxml->html config sxml)
  (let ((page `((html
                 (head
                  (meta (@ (charset "utf-8")))
                  (title ,(hash-table-ref config 'title))
                  (link (@ (rel "stylesheet")
                           (href ,(string-append "/assets/"
                                                 (hash-table-ref config 'theme)
                                                 "/main.css")))))
                 (body
                  ,sxml
                  (footer
                   (strong "LibrePoll")
                   (br)
                   "License: "
                   (a (@ (href
                          "https://www.gnu.org/licenses/agpl-3.0.txt"))
                      "AGPL")
                   (br)
                   "Source code: "
                   (a (@ (href
                          "https://framagit.org/prouby/librepoll"))
                      "Framagit")
                   (br)))))))
    (let ((html (call-with-output-string
                  (lambda (port)
                    (sxml->xml page port)))))
      html)))

(define (poll-info db id)
  (let* ((poll (sql-poll-by-id db (list id)))
         (name (list-ref poll 0))
         (desc (list-ref poll 1)))
    (values name desc)))

(define (render-page-index config db)
  (if (file-exists? "index.html")
      (file-content "index.html")
      (sxml->html config
                  `((header
                     (h1 "Welcome in LibrePoll !"))
                    (div (@ (class "core"))
                         (div (@ (class "description"))
                              ,(commonmark->sxml (hash-table-ref config 'comment)))
                         (h2 "Create new poll")
                         (ul
                          (li (a (@ (href
                                     ,(string-append (hash-table-ref config 'host)
                                                     "/create")))
                                 "Click here")))
                         (h2 "Statistique")
                         (p "There are " (strong ,(sql-vote-count db) " votes")
                            " on this instance." (br)
                            "There are " (strong ,(sql-poll-count db) " polls")
                            " on this instance."))))))

(define (render-page-create-poll-get config db)
  (sxml->html config
              `((header (h1 "Create new poll"))
                (div (@ (class "core"))
                     (form
                      (@ (action "/create")
                         (method "post"))
                      (table (tr (td
                                  "Name" (br)
                                  (input (@ (type "text")
                                            (name "name")))))
                             (tr (td
                                  "Description" (br)
                                  (textarea (@ (name "description")
                                               (rows "24"))
                                            "")))
                             (tr (td (input (@ (type "submit")
                                               (value "Submit")))))))))))

(define (render-page-add-option poll config db)
  (let ((options (sql-options-for-poll db `(,poll))))
    (sxml->html
     config
     `((header (h1 "Add options"))
       (div (@ (class "core"))
            ,(map (lambda (l) `(div (@ (class "option"))
                                    ,(substitute (cadr l) "&apos;"
                                                 "'")))
                  options)
            (form
             (@ (action ,(string-append "/add_option/" poll))
                (method "post"))
             (table (tr
                     (td "Option")
                     (td (input (@ (type "text")
                                   (name "name"))))
                     (td (input (@ (type "submit")
                                   (value "+")))))))
            (div (@ (class "link"))
                 (h2 (a
                      (@ (href ,(string-append (hash-table-ref config 'host)
                                               "/poll/" poll)))
                      ,(string-append (hash-table-ref config 'host)
                                      "/poll/" poll)))))))))

(define (render-page-result poll config db)
  (let-values (((name desc) (poll-info db poll)))
    (let* ((total (sql-vote-number-by-poll db (list poll)))
           (fn (lambda (l)
                 (let* ((num (sql-vote-num-for-option
                              db
                              (list (car l))))
                        (perc (percent num total))
                        (perc-str (string-append (number->string perc) "%")))
                   `((tr (td (div (@ (class ,(if (> perc 50)
                                                 "result-maj"
                                                 "result")))
                                  ,(substitute (cadr l) "&apos;" "'")))
                         (td ,num)
                         (td (div (@ (class ,(if (> perc 50)
                                                 "result-maj"
                                                 "result")))
                                  ,perc-str)))))))
           (pl (lambda (l)
                 (let* ((num (sql-vote-num-for-option
                              db
                              (list (car l)))))
                   (/ (percent num total) 100.0))))
           (opts (sql-options-for-poll db (list poll)))
           (opts-sorted (list-sort (lambda (x y)
                                     (let ((num-x (sql-vote-num-for-option
                                                   db (list (car x))))
                                           (num-y (sql-vote-num-for-option
                                                   db (list (car y)))))
                                       (> num-x num-y)))
                                   opts))
           (opth (map fn opts-sorted))
           (percent-lst (map pl opts-sorted)))
      (sxml->html
       config
       `((header (h1 ,name))
         (div (@ (class "core"))
              (div (@ (class "description"))
                   ,(commonmark->sxml desc))
              (table
               (@ (class "main"))
               (tr
                (td
                 (table
                  (tr
                   (th (em "Options"))
                   (th (em "Votes"))
                   (th (em "Results")))
                  ,opth))
                (td
                 ,(make-pie-chart (delete 0 percent-lst)))))))))))

(define (render-page-poll id config db)
  (let* ((fn (lambda (l)
               `(tr
                 (td (input (@ (type "radio")
                               (name "vote")
                               (value ,(car l)))
                            ,(substitute (cadr l) "&apos;" "'"))))))
         (poll (sql-poll-by-id db (list id)))
         (name (list-ref poll 0))
         (desc (list-ref poll 1))
         (opts (sql-options-for-poll db (list id)))
         (opth (map fn opts)))
    (sxml->html
     config
     `((header (h1 ,name))
       (div (@ (class "core"))
            (div (@ (class "description"))
                 ,(commonmark->sxml desc))
            (form
             (@ (action
                 ,(string-append
                   "/result/" id))
                (method "post"))
             (table
              ,opth
              (tr
               (td (input (@ (type  "submit")
                             (value "Vote"))))))))))))
