# JSF just stands for JavaScript Framework

Check out this `README.md`, and the development demo at `index.html`.

## (Limited) Demo

Open `jsfTests.html` in a browser, then pop open the developer console to see
printed messages.

Remember to host the directory containg `.html` files on a webserver, as [CORS
security protocol will not allow your browser to simply open Javascript files
from your
filesystem](https://stackoverflow.com/questions/46992463/es6-module-support-in-chrome-62-chrome-canary-64-does-not-work-locally-cors-er).

### Demo: Example Web Service (Python 3, Linux)

```
sudo python3 -m http.server 80  2> /dev/null &

# You may have to run this once, bring it to the foreground with `fg`,
# authenticate your superuser, `Ctrl+c` to kill the process, then run it again.
```

## Versions

### Versions: Current branch

We're trying to build Datum, as a type of Actor.

### Versions: Upstream

v0.0.1 - representing a working version of the messaging infrastructure, albeit
with clunky implementation.

Actors can now message each other via the Postman. Actors can therefore be
configured to respond to mail by calling `addEventListener()` and
`removeEventListener()` on themselves, or by calling `despatchEvent()` on
themselves. We expect to see `CustomEvent` used in this way - the whole point of
"messaging" is that an actor should never make these calls on
another actor, instead, only sending messages requesting that other actors do as
such. **

### Versions: Backlog Progress

Done:
- window.actorRegistry
- Actor superclass
- - this class extends EventTarget
- - Actors have inboxes
- Postman subclass
- - Postman determines messaging protocol between Actors
- - each window.actorRegistry can have one Postman, at most
- - each Postman carries another internal registry of message recipients; this
  is uniquely keyed by actor.identity 
- - when a Postman is instantiated, it adds unique Actor IDs from
  window.actorRegistry to (new Postman).recipientRegistry, then overwrites a bit
  of the Actor class to cause all future instatiations of Actor to register
  themselves to (new Postman).recipientRegistry on construction.
- - messages are Objects with symbol keys to reduce accidental mucking up

### Versions: Disclaimers (Technical Debt)

This is not a good example of OOP separation of concerns, in terms of how the
functions were named.

Tests are not professionally written; these are minimally viable tests, which
would require further grooming to be maintainable by a new team.

### Versions: Next Thoughts

Given the messaging functionality achieved above (which could be grossly more
performant after optimisation) I need to think briefly about how reactive data
storage should behave around this.

MODELS:

-   Declarative object syntax could result in one actor per datum, such that
    changes in data are implemented as CustomEvents on actors.

-   You could thus 

-   -   message an actor (A_n) to broadcast state changes by dispatching events upon itself

-   -   message other actors (A_m) to listen to those events on an (A_n)

-   -   invalidate caches in (A_m)s, based on events in (A_n)

-   -   cache invalidations may or may not trigger rerendering (configured, per
        actor cache)

VIEWS:

-   Declarative object syntax could result in one actor per DOM component, such
    that changes in views are implemented as CustomEvents on actors.

-   -   user inputs may or may not trigger rerendering (configured, per actor
    component)

RENDERING IN GENERAL (perhaps incoherent):

-   Segregate between subtrees of the DOM which are meant to be static (B_o), and
    subtrees which are meant to be dynamic (B_p).

-   -   Track (B_o)s and (B_p)s in a tree of actors

-   -   Diff only (B_p)s for dynamic changes, ignoring (B_o)s
        ([lit-html](https://www.youtube.com/watch?v=ruql541T7gc),
        [hyperHTML](https://gist.github.com/WebReflection/ab43649d9e4a53ac900b5924c77a310e]))

-   When checking (B_p)s for diffs, do it in a virtual DOM first.

-   -   When dirty data is detected in (A_m)s, rerender only the relevant
        subtrees. ([React,
        Vue](https://bitsofco.de/understanding-the-virtual-dom/),
        [Hyperapp](https://github.com/jorgebucaran/hyperapp))  

-   Edgy:

-   -   How about having generic code handle declarative configuration, WHILE
    allowing a background optimisation... a compiler which spits out more
    optimised code which is run in eval(), this code being cached for future
    use, until the next change in declarative configuration. ([Svelte,
    Sapper](https://svelte.dev/blog/svelte-3-rethinking-reactivity)) 

## Architecture

### Architecture: Leanings

-   I think a build-step should be optional. 
-   I think server-side rendering optimisations should be optional. Hydration
    should be optional (e.g. SSR with and without hydration).
-   I think any new DSLs that aren't VanillaJS should be optional - this
    includes things like Typescript, compiler hints/decorators, and templating
    syntax.

### Architecture: Tools

I like Vim.

### Architecture: Motivations

Currently a study of 2015-2019 JavaScript developments I've not been keeping up with.

After reviewing a number of reactive-style web frameworks, I wasn't clear on how each 
one was architectured.

Instead of reading all the code for all of those, I decided to write a brief study in 
the form. The architecture being developed here is loosely based on EventTarget and
Erlang's [VM-actor-messages
model](http://erlang.org/doc/getting_started/conc_prog.html).

** (I'm not sure if you should call this a polite protocol, but Erlang was
designed by a Brit.) 

> v0.0.1 - we now have (only) inter-actor messaging! The direction here is to
> use Vanilla.js (NO required templating language, compiler decorators, or
> TypeScript, nothing ex-ES!), NO required build step (optional SSR, optional
> compilation of configuration into code at runtime, and cached compiled code
> run in eval()), REACTIVITY... every datum ('field') will be modelled as an
> Actor (extension of EventTarget) that can receive messages, and therefore each
> Actor can use Event to talk to other Actors. The intention is to build this so
> that server side Actors and client side Actors can message each other, and
> that each light-weight Actor represents finely grained model-states (down to
> the field) or view-states (down to the element). If you read so far, thanks
> for following this noob attempt to brain 2019 JS features...
