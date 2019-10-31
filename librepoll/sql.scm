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

(define-module (librepoll sql)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 format) #:prefix icef:)
  #:use-module (sxml simple)
  #:export (define-sql-request))

(define-syntax-rule (define-sql-request name DOCSTRING parser sql)
  "Create new procedure named NAME. If PARSER is a procedure,
wrap the defaut returns, else do nothing. SQL is sql request in string
format."
  (define* (name db #:optional (args '()))
    DOCSTRING
    (let* ((core (if (null-list? args)
                     (lambda ()
                       ;; (log (format "sql: ~a" sql))
                       (exec-sql->list db sql))
                     (lambda ()
                       (let* ((l (map
                                  (lambda (s)
                                    (if (string? s)
                                        (substitute s "'"
                                                    "&apos;")
                                        s))
                                  args))
                              (str (apply format (cons sql l))))
                         ;; (log (format "sql: ~s\n" str))
                         (exec-sql->list db str)))))
           ;; Wrap core with parser if parser is define.
           (exec (cond
                  ((procedure? parser)
                   (lambda ()
                     (parser (core))))
                  (else
                   core))))
      (exec))))
