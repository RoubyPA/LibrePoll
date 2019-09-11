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

(define-module (librepoll create-db)
  #:use-module (librepoll sql)
  #:use-module (sqlite3)
  #:export (create-db))

(define (create-db db file)
  (define (sql-request sql)
    (let ((stmt (sqlite-prepare db sql)))
        (sqlite-fold cons '() stmt)))

  (sql-request
   "CREATE TABLE IF NOT EXISTS poll (
     id INTEGER PRIMARY KEY,
     name CHARACTER(50) NOT NULL,
     description TEXT,
     creation_date TIME NOT NULL
   );")

  (sql-request
   "CREATE TABLE IF NOT EXISTS option (
     id INTEGER PRIMARY KEY,
     poll INTEGER NOT NULL,
     name VARCHAR(64) NOT NULL,
     FOREIGN KEY(poll) REFERENCES poll(id)
   );")

  (sql-request
   "CREATE TABLE IF NOT EXISTS vote (
     id INTEGER PRIMARY KEY,
     date TIME NOT NULL,
     poll INTEGER NOT NULL,
     option INTEGER NOT NULL,
     metadata TEXT NOT NULL,
     FOREIGN KEY(option) REFERENCES option(id),
     FOREIGN KEY(poll) REFERENCES poll(id)
   );")

  (sql-request
   (format "INSERT INTO poll (id,name,description,creation_date)
     VALUES (
       1,
       'Do you like this software ?',
       '~a',
       DATETIME('now') );"
           "This program is free software, licensed under
[AGPL](http://www.gnu.org/licenses/).

```
This program is free software: you can redistribute it and/or
modify it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program.  If not, see
<http://www.gnu.org/licenses/>.
```
"))

  (sql-request
   "INSERT INTO option (poll,name)
     VALUES (1, 'Yes');")

  (sql-request
   "INSERT INTO option (poll,name)
     VALUES (1, 'Non');"))
