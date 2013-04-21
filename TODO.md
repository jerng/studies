# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

### Missing Defaults

### Miscellany

TOP PRIORITY:
Framework already does most hard stuff.
Make data access more ergonomic.
See http://localhost:3000/test/form and proceed.

Implement a URL helper.
Implement a simple form helper.
Implement authentication.

Make giant re-exportation library to turn T.append into tAppend, 
  BS.append into bsAppend, etc.
Make Report showable.
Look into ResponseSource usage for Hell.Server.respond

Make ./src/* file names case insensitive.

Replace all association lists, with Data.Map.Map
user Builders properly! 

Make a session helper
Make a cookie helper

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

In Hell.Conf, (controllers), (views), etc. should return a list of type
(HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course,
first define this in Hell.Types

Eviscerate common errors with no messages / uninformative messages.
  Beware incidences of (fromJust)
Write tests
Write documentation

## Development is currently on

Ubuntu 12.04 LTS 64-bit
GHC 7.4.2
"sudo apt-get install haskell-platform" gets Cabal-1.16.0.3

### After reinstallations ...

... of GHC and Haskell-Platform, ALWAYS DELETE `~/.cabal` and `~/.ghc`
