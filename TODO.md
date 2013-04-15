# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

### Missing Defaults

### Miscellany

Parse GET and POST queries
Implement a URL helper.
Implement a simple form helper.

Rewire Views so that Reports never get sent to View.
Perhaps only send Report { viewBson } to Views.
Consider sending only Text, Int, and Float, to views. 
  (Perhaps a new type, limited BSON)

Implement authentication.

Make giant re-exportation library to turn T.append into tAppend, 
  BS.append into bsAppend, etc.
Make Report showable.
Look into ResponseSource usage for Hell.Server.respond

Make ./src/* file names case insensitive.

Cabalise this.
Replace all association lists, with Data.Map.Map
user Builders properly! (no idea about this yet)

Wherever Document is used, include a type synonym as an abstraction "interface"

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
