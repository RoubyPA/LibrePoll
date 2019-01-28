LibrePoll
=========

Free opinion poll service.


LibrePoll provide a web interface to create opinion poll with Markdown
description.


## Dependencies

 - Guile >= 2.2
 - Guile-json
 - Guile-sqlite3
 - Guile-commonmark
 - Guile-syntax-highlight


## Test LibrePoll in sources

Run librepoll with default configuration.

```shell
$ guile librepoll
```

Change configuration file.

```shell
$ guile librepoll my/config/file.json
```


## Install

Use guix.

```shell
$ guix package -f guix
```


## Run

After guix install.

```shell
$ librepoll [CONFIGFILE]
```


## Configuration

By default LibrePoll use `librepoll.json` in current dir as
configuation file.

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
| title    | Page title                                                  |
| theme    | Theme to use in html                                        |
| comment  | Message to display in the index page (Markdown)             |


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
    "title"    : "LibrePoll",
    "theme"    : "default",
    "comment"  : "Message to display in the index page."
}
```
