# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

### Missing Defaults

### Miscellany

Make an instance of Show Request
Make a debug helper
Iron out how debugging should work

Make a session helper
Make a cookie helper

Make ./src/* file names case insensitive.

Replace all association lists, with Data.Map.Map

Implement a helper function for populating the ViewDictionary in Controllers.
Implement various types of ResponseBuilder in Hell.Server.render
Following these, implement authentication.
Implement redirection, from one action, to another action.
Implement a URL helper.
Implement a simple form helper.

## Design questions  

Currently:

Under consideration:

## Functional limits to-be-examined

Can all View code share scope? (e.g. all run within a do {})

Where (and how) can Text be replaced with Builders (since all Text is
ultimately converted to Builder)?

## Chore-like:

Eviscerate common errors with no messages / uninformative messages.

Once Models are in, Controllers which are now pure will become Monadic D:

In Hell.Conf, (controllers), (views), etc. should return a list of type
(HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course,
first define this in Hell.Types
