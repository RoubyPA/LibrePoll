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

(define-module (librepoll options)
  #:use-module (librepoll tools)
  #:use-module (librepoll error)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-69)
  #:use-module (json)
  #:export (share-path
            get-options-ht))

(define share-path
  (let ((dir (getenv "LIBREPOLL_ASSETS_DIR")))
    (if (string? dir)
        dir
        "")))

;;;
;;; Public
;;;

(define (get-options-ht json-name)
  (define (conf-ref conf key)
    "Use 'hash-ref' if CONF is hash-table, else 'assoc-ref'."
    (cond
     ((hash-table? conf)
      (hash-ref conf key))
     (else
      (assoc-ref conf key))))

  (define conf
    (if (file-exists? json-name)
        (let ((json (file-content json-name)))
          (json-string->scm json))
        (abort-with-msg "No config file")))

  (alist->hash-table
   `((db-file  . ,(conf-ref conf "db"))
     (host     . ,(conf-ref conf "host"))
     (port     . ,(conf-ref conf "port"))
     (security . ,(conf-ref conf "security"))
     (api      . ,(string= (conf-ref conf "api") "true"))
     (ui       . ,(string= (conf-ref conf "ui")  "true"))
     (log-file . ,(conf-ref conf "log"))
     (stdout   . ,(string= (conf-ref conf "stdout") "true"))
     (title    . ,(conf-ref conf "title"))
     (theme    . ,(conf-ref conf "theme"))
     (comment  . ,(conf-ref conf "comment")))))
