# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

### Missing Defaults

### Miscellany

Make ./src/* file names case insensitive.

Start using BSON everywhere...

Replace all association lists, Hell.Types.DM, with Data.Map.Map?

Session handling using Web.ClientSession

Implement a URL helper.
Implement redirection, from one action, to another action.
Implement a simple form helper.
Implement a helper function for populating the ViewDictionary in Controllers.
Implement various types of ResponseBuilder in Hell.Server.render

Customise ResponseHeaders in (Hell.Server.render)
  Cookie handler
      Following these, implement authentication.

Implement static file service.

## Design questions  

Currently:

  Hell.Server.main
    app
      report
        (confirmA) >--------------------------+
                     ^                        |
                     +---------------------+  |
          router                           |  |
            (Controllers.controller.action)+  |
        applyAtoR                                  |
          (AppController.main) <--------------+
      render -- subreports? --> goto (confirmA,applyAtoSubR)...
                                  until Text is returned
        reportToText

Under consideration:

## Functional limits to-be-examined

Can all View code share scope? (e.g. all run within a do {})

Where (and how) can Text be replaced with Builders (since all Text is
ultimately converted to Builder)?

## Chore-like:

Once Models are in, Controllers which are now pure will become Monadic D:

In Hell.Conf, (controllers), (views), etc. should return a list of type
(HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course,
first define this in Hell.Types
