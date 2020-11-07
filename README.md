# Nextup

Nextup is a tiny tool for managing a "to-read/to-watch/to-listen" queue. I doubt it will be useful to anyone
apart from myself but you never know.

```
Usage: nextup COMMAND
  Manage your media queue

Available options:
  -h,--help                Show this help text

Available commands:
  stats                    Get statistics about your media library
  add                      Add a new media item
  next                     Get the next unrated item
  list                     List all items of a particular format
  rate                     Rate an item
```

An example usage session:

```
nextup add Album Discovery "Daft Punk"

  [Added]
  (3) Discovery - Daft Punk (?) [Album]

nextup add Book "The Terror" "Dan Simmons"

  [Added]
  (4) The Terror - Dan Simmons (?) [Book]

nextup list Album

  (1) Summer Mirage - Chester Watson (?) [Album]
  (2) Fires in Heaven - Salem (?) [Album]
  (3) Discovery - Daft Punk (?) [Album]

nextup next Album # Next unrated / unlistened album

  (1) Summer Mirage - Chester Watson (?) [Album]

nextup rate 1 3
nextup list Album

  (1) Summer Mirage - Chester Watson (3) [Album]
  (2) Fires in Heaven - Salem (?) [Album]
  (3) Discovery - Daft Punk (?) [Album]
```

## Installation

```
git clone https://github.com/kdelwat/nextup.git
cd nextup
cabal install
```

## Configuration

You can set the database / CSV file location using Nextup's config file, which will be created by default at
`$XDG_CONFIG_DIR/nextup/nextup.ini`:

```
[DATABASE]
path: /home/myusername/nextup.csv
```

By default, the database will be at `nextup_db.csv` in the directory in which you run the program.
