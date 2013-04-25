# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

### Missing Defaults

### Miscellany

Implement a simple form generator (parser is mostly done).
Implement authentication.
Link up data_ passing from models and to views.

Make Report showable.
Look into ResponseSource usage for Hell.Server.respond

Replace all association lists, with Data.Map.Map
use Builders properly! 

Make a session helper
Make a cookie helper

Improve how view data lookups behave.


## Design questions  

Currently Stati:

Stati Being Considered:

## Functional limits to-be-examined

Can all View code share scope? (e.g. all run within a do {})

Where (and how) can Text be replaced with Builders (since all Text is
ultimately converted to Builder)?

Caching and/or keeping state, to share data across request-response pairs
( refer to note in Hell.Conf.warpServer)

## Chore-like:

Make ./src/* file names case insensitive.

In Hell.Conf, (controllers), (views), etc. should return a list of type
(HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course,
first define this in Hell.Types

Comb through functions in the Hell.X namespace. 
There's no point in censoring things for security purposes, since
developers who really want to use particular functions will import them
anyway.
Consider it nevertheless.

Eviscerate common errors with no messages / uninformative messages.
  Beware incidences of (fromJust)
Write tests
Write documentation

DSL?
Make giant re-exportation library to turn T.append into tAppend, 
  BS.append into bsAppend, etc.

## Development is currently on

Ubuntu 12.04 LTS 64-bit
GHC 7.4.2
"sudo apt-get install haskell-platform" gets Cabal-1.16.0.3

### After reinstallations ...

... of GHC and Haskell-Platform, ALWAYS DELETE `~/.cabal` and `~/.ghc`
