# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

### Missing Defaults

### Miscellany

Make giant re-exportation library to turn T.append into tAppend, 
  BS.append into bsAppend, etc.
Make Report showable.
Make a session helper
Make a cookie helper
Following these, implement authentication.

Look into ResponseSource usage for Hell.Server.respond
Implement redirection, from one action, to another action.
Implement a URL helper.
Implement a simple form helper.

Make ./src/* file names case insensitive.

Replace all association lists, with Data.Map.Map
user Builders properly! (no idea about this yet)

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
