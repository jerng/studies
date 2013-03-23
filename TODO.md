# To-dos

## Laundry List of outstanding tasks; sorted by ease, descending

Deal with missing keys in ViewDictionary (currently: crashes without much description).

Automate assembly of Hell.Server.actionList and .viewList

Implement a helper function for populating the ViewDictionary in Controllers.

Implement redirection, from one action, to another action.

Implement a simple form helper.

Implement a URL helper.

Customise ResponseHeaders in (Hell.Server.render)

Check functionality of Warp sessions. Check functionality of Warp cookies.  Following these, implement authentication.

## Design questions  

Currently:

    (run) -> (app) -> (render) <-> (getAction) <-+-> (router)
                          |           |          |
                          |           |          +-> (actionList)
                          |           |
                          |           +-> (Controller.Action)
                          |
                          +-------> (Controller.View)

Under consideration:

    (run) -> (app) -> (render) <-> (getAction) <-> (App("global")Controller) <-+-> (router)
                          |                                    |               |
                          |                                    |               +-> (actionList)
                          |                                    |
                          |                                    +-> (Controller.Action)
                          |
                          +-------> (Controller.View)


## Functional limits to-be-examined

Can all View code share scope? (e.g. all run within a do {})

Where (and how) can Text be replaced with Builders (since all Text is ultimately converted to Builder)?

## Chore-like:

Replace all association lists, with Data.Map

In Hell.Conf, (controllers), (views), etc. should return a list of type (HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course, first define this in Hell.Types
