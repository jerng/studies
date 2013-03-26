# To-dos

## Laundry List of outstanding tasks; sorted by ease*importance, descending

TODO: have the rendered create a default routeV if it's not specified

Implement meta-View (CakePHP' "setFlash")

Implement redirection, from one action, to another action.

Implement a helper function for populating the ViewDictionary in Controllers.

Implement a simple form helper.

Implement a URL helper.

Customise ResponseHeaders in (Hell.Server.render)

Check functionality of Warp sessions. Check functionality of Warp cookies.  Following these, implement authentication.

## Design questions  

Currently:

  Hell.Server.main
    app
      report
        (confirmAction)
          router
            (Controllers.controller.action)
        applyActionToReport
      render -- subreports? goto (confirmAction)... until Text is returned
        reportToText

Under consideration:

  Add (AppController) between (router) and (Controllers.controller.action)

## Functional limits to-be-examined

Can all View code share scope? (e.g. all run within a do {})

Where (and how) can Text be replaced with Builders (since all Text is ultimately converted to Builder)?

## Chore-like:

Paths like localhost/test/ route properly, but paths like localhost/test do not.

Replace all association lists, Hell.Types.DM, with Data.Map.Map

In Hell.Conf, (controllers), (views), etc. should return a list of type (HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course, first define this in Hell.Types
