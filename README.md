## OegySync

A simple `rsync` wrapper that let's you configure you synced directories
in a JSON file and then just run `oegysync`.

## Install

You need `ghc`. The easiest way, on a Mac might be

    brew install haskell-platform

Then install `oegysync` using `cabal`

    cd oegysync
    cabal install

Now if you're lucky, you've got `oegysync` on your path. If not, put is
on your path.

## Configure

Put a file `.oegysyncrc` in your home directory. Something like this.

```json
{
  "root": {
    "local": "/Users/juha",
    "remote": "/Volumes/RemoteDrive"
  },
  "paths": [
    { "local": "Pictures/2013", "remote": "Pictures/2013" }
  ]
}

Then run `oegysync`. It will sync the directories both ways. Won't
delete anything though.
