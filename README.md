LibrePoll
=========

Free opinion poll service.

---

## Dependencies

 - Guile <= 2.2
 - Guile-json
 - Guile-sqlite3
 - Guile-commonmark

## Run LibrePoll server

Run librepoll with default configuration.

```shell
$ guile librepoll
```

Change configuration file.

```shell
$ guile librepoll my/config/file.json
```


## Configuration

By default LibrePoll use `librepoll.json` as configuation file.

| Options  | Usage                                                       |
|----------|-------------------------------------------------------------|
| db       | Database file                                               |
| host     | Host name (use for display)                                 |
| port     | Http server port                                            |
| security | Type of security (at this time just cookies is implemented) |
| api      | Make api available ("true" or "false")                      |
| ui       | Make ui available ("true" or "false")                       |
| stdout   | Display log in stdout ("true" or "false")                   |
| log      | Log file                                                    |
| comment  | Server comment (for display)                                |


```json
{
    "db"       : "librepoll.sqlite",
    "host"     : "localhost",
    "port"     : 8081,
    "security" : "cookies",
    "api"      : "true",
    "ui"       : "true",
    "stdout"   : "true",
    "log"      : "librepoll.log",
    "comment"  : "## This is LibrePoll !\n
\n
 1. Create new poll\n
 2. Add options\n
 3. Share the links\n"
}
```
