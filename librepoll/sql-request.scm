(define-module (librepoll sql-request)
  #:use-module (librepoll tools)
  #:use-module (librepoll sql)
  #:use-module (srfi srfi-1)
  #:use-module (sqlite3)
  #:export (exec-sql->list
            ;; Request
            sql-vote-count
            sql-poll-count
            sql-vote-number-by-poll
            sql-polls
            sql-poll-by-id
            sql-poll-by-name
            sql-options-for-poll
            sql-vote-for-poll
            sql-vote-num-for-option
            sql-vote-exist?
            sql-vote
            sql-create-poll
            sql-create-option-for-poll))

(define (exec-sql->list db sql)
  (define (vector-list->list vl)
    (cond
     ((null-list? vl) vl)
     ((vector? vl)    (vector->list vl))
     ((list? vl)      (map vector->list vl))
     (else            '())))

  (let ((stmt (sqlite-prepare db sql)))
    (vector-list->list (sqlite-fold cons '() stmt))))

(define-sql-request
  sql-vote-count
  "Returns number of all vote in db."
  (lambda (l) (caar l))
  "SELECT COUNT(*) FROM vote;")

(define-sql-request
  sql-poll-count
  "Returns number of all poll in db."
  (lambda (l) (caar l))
  "SELECT COUNT(*) FROM poll;")

(define-sql-request
  sql-vote-number-by-poll
  "Count all vote for poll."
  (lambda (l) (caar l))
  "SELECT COUNT(*) FROM vote WHERE poll = ~a;")

(define-sql-request
  sql-polls
  "Returns list of all poll in db."
  'nil
  "SELECT id,name,description FROM poll;")

(define-sql-request
  sql-poll-by-id
  "Returns poll coresponding to ID in db."
  (lambda (l) (car l))
  "SELECT name,description FROM poll WHERE id = ~a;")

(define-sql-request
  sql-poll-by-name
  "Returns poll id."
  (lambda (l) (caar l))
  "SELECT id FROM poll WHERE name = '~a';")

(define-sql-request
  sql-options-for-poll
  "Returns list contain poll for first element of list."
  (lambda (l) (reverse l))
  "SELECT id,name FROM option WHERE poll = ~a;")

(define-sql-request
  sql-vote-for-poll
  "Returns list of all vote for poll."
  'nil
  "SELECT id,option,date,metadata FROM vote WHERE poll = ~a;")

(define-sql-request
  sql-vote-num-for-option
  "Returns count of vote for poll."
  (lambda (l) (caar l))
  "SELECT COUNT(*) FROM vote WHERE vote.option = ~a;")

(define-sql-request
  sql-vote-exist?
  "Returns true if vote already exists."
  (lambda (l) (not (zero? (caar l))))
  "SELECT COUNT(*) FROM vote
     WHERE vote.poll = ~a AND
           vote.metadata = ~s;")

(define-sql-request
  sql-vote
  "Insert new vote in db."
  'nil
  "INSERT INTO vote (date, poll, option, metadata)
     VALUES (DATETIME('now'), ~a, ~a, ~s);")

(define-sql-request
  sql-create-poll
  "Insert new poll in db."
  'nil
  "INSERT INTO poll (name,description,creation_date)
     VALUES('~a', '~a' ,DATETIME('now'));")

(define-sql-request
  sql-create-option-for-poll
  "Insert new poll option in db."
  'nil
  "INSERT INTO option (poll,name)
     VALUES(~s, ~s);")
