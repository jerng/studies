# To-dos

## Laundry List of outstanding tasks; sorted by ease, descending

Implement Views-in-Views ("widgets")

Implement meta-View (CakePHP' "setFlash")

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
                          |                                    +-> (Controller.main)
                          |                                                |
                          |                                                +-> Controller.actor
                          |
                          +-------> (Controller.View)


## Functional limits to-be-examined

Can all View code share scope? (e.g. all run within a do {})

Where (and how) can Text be replaced with Builders (since all Text is ultimately converted to Builder)?

## Chore-like:

Paths like localhost/test/ route properly, but paths like localhost/test do not.

Replace all association lists, Hell.Types.DM, with Data.Map.Map

In Hell.Conf, (controllers), (views), etc. should return a list of type (HellResource { name :: x ,  type ::, fromPath ::, toPath ::} ; of course, first define this in Hell.Types
