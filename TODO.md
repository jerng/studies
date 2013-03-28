# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

### Missing Defaults

### Miscellany

Implement a URL helper.
Implement redirection, from one action, to another action.
Implement a simple form helper.
Implement a helper function for populating the ViewDictionary in Controllers.
Implement various types of ResponseBuilder in Hell.Server.render

Customise ResponseHeaders in (Hell.Server.render)
  Cookie handler
    Session handling using Web.ClientSession
      Following these, implement authentication.

## Design questions  

Currently:

  Hell.Server.main
    app
      report
        (confirmAction) >---------------------+
                     ^                        |
                     +---------------------+  |
          router                           |  |
            (Controllers.controller.action)+  |
        applyActionToReport                   |
          (AppController.main) <--------------+
      render -- subreports? --> goto (confirmAction,applyActionToSubReport)...
                                  until Text is returned
        reportToText

Under consideration:

## Functional limits to-be-examined

Can all View code share scope? (e.g. all run within a do {})

Where (and how) can Text be replaced with Builders (since all Text is
ultimately converted to Builder)?

## Chore-like:

Once Models are in, Controllers which are now pure will become Monadic D:

Replace Hell.Server.actionList and viewList with case-ofs

Replace all association lists, Hell.Types.DM, with Data.Map.Map

In Hell.Conf, (controllers), (views), etc. should return a list of type
(HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course,
first define this in Hell.Types
