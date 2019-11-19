# scrbl

## Description
`scrbl` is a stupid little application that lets me manage my quick scribbles (aka notes) in a hierarchal way. Notes are of any specified filetype, and are opened in the user's `$EDITOR`, or configured binary. `scrbl` exposes a natural, frictionless interface for handling and organizing notes.

## Usage
```bash
$ scrbl --help
scrbl [--help|-h] [--config CONFIG] [segments...] scribble_file[.extension]
```

## Examples
```bash
$ scrbl --help || scrbl -h
$ scrbl my_new_scribble
$ scrbl supergroup subgroup my_scribble # creates <BASE>/supergroup/subgroup/note
```

## Configuration
Configuration is handled via a JSON file. Although a configuration file can be specified to `scrbl`, generally, this is not recommended as it makes usage of `scrbl` clunky. Instead, when called without `--config`, `scrbl` will look for this JSON configuration file in the following places, in order of priority descending:
    1. `~/.config/scrbl/scrbl.json`
    2. `~/.scrbl.json`
    3. `~/etc/scrbl/scrbl.json`

The JSON configuration file is short and simple, because `scrbl` is simple. A description of this JSON file is as follows.
```json
{
    "base": <filepath(directory)>,
    "editor": <binary name|filepath(binary)>,
    "default_extension": <dot-extension>,
    "accept_paths": true|false,
    "sync": {
        "ssh": {
            "host": <hostname>,
            "port": <port>,
            "path": <filepath(directory)>
        },
        "local": {
            "path": <filepath(directory)>
        }
    }
}
```
Below is some extra information on the fields shown above:
| Field               | Format                         | Description                                                  | Examples             |
|:-------------------:|:------------------------------:|:------------------------------------------------------------:|:--------------------:|
|`"base"`             |`<filepath(directory)>`         |The root directory of scribble hierachies.                    |    -                 |
|`"editor"`           |`<binary name|filepath(binary)>`|Binary name if on `$PATH`, absolute path otherwise.           |`code`,`/usr/bin/nvim`|
|`"default_extension"`|`<dot-extension>`               |The default extension to use for scribbles when not specified.|`".md"`,`".txt"`,`""` |
|`"accept_paths"`     |`true|false`                    |Whether scribble names can be paths.                          |-|
|`"sync.ssh.host"`    |`<hostname>`                    |The hostname of the `ssh` server.                             |-|
|`"sync.ssh.port"`    |`<port>`                        |The port of the `ssh` server.                                 |-|
|`"sync.ssh.path"`    |`<filepath(directory)>`         |The path on the `ssh` server.                                 |-|
|`"sync.local.path"`  |`<filepath(directory)>`         |The path on the local filesystem to backup saved scribbles to.|-|