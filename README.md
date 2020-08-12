# scrbl

## Description
`scrbl` is a stupid little application that lets me manage my quick scribbles
(aka notes) in a hierarchal way. Notes are of any specified filetype, and are
opened in the user's `$EDITOR`, or configured binary. `scrbl` exposes a
natural, frictionless interface for handling and organizing notes.

At some point I may add some syncing feature to something remote, e.g., Google
drive or a server via `scp`. Thus far, my notes tend to be so ephemeral that
this has not really been a priority of mine.

## Note on the code
I did this project largely for the sake of getting some practical Haskell
experience, i.e., writing some "real-world" Haskell code. As a result, there
are some design choices here that are probably not good at all, such as
hand-coding command line argument parsing instead of using a dependency like
`parseargs`, or writing an excessive amount of test code to familiarize myself
with `HUnit`.

## Usage
```bash
$ scrbl --help
scrbl [--help|-h] [--config CONFIG] [segments...] scribble_file[.extension]
```
*NOTE*: The order of arguments, except for `--help`/`-h` is _not_ flexible. If
you specify a `--config`, it _must_ come before, e.g., `[segments...]`. This is
due to the fact that it is ambiguous whether `--config CONFIG` refers to a
segment or is a flag, thanks to Unix file name rules allowing such characters.
This is likely fixable if I had used someone's better-and-more-robustly written
library for argument parsing, but I wanted to get more practice with Haskell,
so here we are.

## Examples
```bash
$ scrbl --help || scrbl -h
$ scrbl my_new_scribble
$ scrbl supergroup subgroup my_scribble # creates <BASE>/supergroup/subgroup/note
```

## Configuration
Configuration is handled via a JSON file. Although a configuration file can be
specified to `scrbl`, generally, this is not recommended as it makes usage of
`scrbl` clunky. Instead, when called without `--config`, `scrbl` will look for
this JSON configuration file in the following places, in order of priority
descending:
    1. `~/.config/scrbl/scrbl.json`
    2. `~/.scrbl.json`
    3. `/etc/scrbl/scrbl.json`

The JSON configuration file is short and simple, because `scrbl` is simple. A
description of this JSON file is as follows.
```json
{
    "base": "<filepath(directory)>",
    "editor": "<binary name|filepath(binary)>",
    "extension": "<dot-extension>"
}
```
Below is some extra information on the fields shown above:

| Field       | Examples   | Description                                       | Default            |
|:------------|:----------:|:------------------------------------------------- |:------------------:|
|`"base"`     |`/notes/`   |The root directory of scribble hierachies.         |`~/Documents/scrbl/`|
|`"editor"`   |`vim`,`code`|Binary name if on `$PATH`, absolute path otherwise.|`$EDITOR`           |
|`"extension"`|`""`,`".md"`|The extension to use for files when missing.       |`.txt`              |
