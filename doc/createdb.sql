--
-- Poll
--

CREATE TABLE IF NOT EXISTS poll (
  id            INTEGER PRIMARY KEY,    -- Id unique number
  name          CHARACTER(50) NOT NULL, -- Name of poll
  description   TEXT,                   -- Description of poll
  creation_date TIME NOT NULL           -- Creation date
);

--
-- Option
--

CREATE TABLE IF NOT EXISTS option (
  id       INTEGER PRIMARY KEY,
  poll     INTEGER NOT NULL,     -- Id of poll
  name     VARCHAR(64) NOT NULL, -- Option text
  FOREIGN KEY(poll) REFERENCES poll(id)
);

--
-- Vote
--

CREATE TABLE IF NOT EXISTS vote(
  id       INTEGER PRIMARY KEY,
  date     TIME NOT NULL,        -- Vote date
  poll     INTEGER NOT NULL,     -- Id of poll
  option   INTEGER NOT NULL,     -- Id of option
  metadata TEXT NOT NULL,        -- To verify no duplication of vote
  FOREIGN KEY(option) REFERENCES option(id),
  FOREIGN KEY(poll) REFERENCES poll(id)
);
