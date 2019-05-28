LibrePoll
=========

Free opinion poll service.


LibrePoll provide a web interface to create opinion poll with Markdown
description.


![example](/doc/example.png)


## Dependencies

 - Guile >= 2.2
 - Guile-json
 - Guile-sqlite3
 - Guile-commonmark


## Install

### Autotools

```shell
$ ./bootstrap
$ ./configure
$ make
$ make install
```

### GNU Guix

```shell
$ guix package -f guix
```


## Run

Before installation.

```shell
$ ./pre-inst-env librepoll [CONFIGFILE]
```

After installation.

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
| theme    | Theme to use in html (default, blue, purple, none)          |
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

## API

### `GET /api/v1/status`

Get instance status.

| Name    | Value                       | Type   |
|---------|-----------------------------|--------|
| status  | Always "OK"                 | String |
| license | License name (AGPL)         | String |
| host    | Host name                   | String |
| votes   | Number of vote on incetance | Int    |
| polls   | Number of poll on incetance | Int    |

### `GET /api/v1/poll/:id`

Get instance poll by `:id`.

| Name        | Value               | Type              |
|-------------|---------------------|-------------------|
| name        | Name of poll        | String            |
| description | Description of poll | String (markdown) |
| options     | List of options     | List              |

### `GET /api/vote/:poll/:opt`

New vote for `:poll` and `:opt` (option id).

This api use session cookies (*librepoll_auth*) for authentification.

| Name   | Value       | Type   |
|--------|-------------|--------|
| status | Always "Ok" | String |
