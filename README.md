# Hell

An aspiring MVC web application development framework, for the Haskell
programming platform. A toy project for study.

### Dockerised (2015)

1. Build the Dockerfile
2. Run it with (-p 3000:3000)
3. View it a number of framework features at (host:3000/default)

### Infrastructure

1.  Ubuntu (a Linux distribution)
    
    ### Language Platform    

    1.  `sudo apt-get install haskell-platform` or install Haskell Platform
        from sources.  (when compiling GHC from source, some soft linking of
        `/usr/lib/*.so*` files might be required; e.g.
        `/usr/lib/x86somethingseomthing/from` `/usr/lib/to`)
    2.  `sudo cabal update`
    3.  `sudo cabal install cabal-install`
    4.  `sudo cabal install cabal-dev`
    5.  edit `~/.bashrc` and add `export PATH=~/.cabal/bin/:$PATH` or do whatever else it takes to refer to the `cabal-dev` executable, later

    ### Database

    1.  `sudo apt-get install mongodb-server` or install MongoDB some other way
    2.  Get the `mongod` daemon up and running (some Googling may be required if it doesn't "just work").
    3.  `mongo` to enter the mongo shell (success will prove that the daemon is working)
    4.  In the shell: `use testdb` to create a new database with this name.
    5.  In the shell: `db.createCollection("testcollection")`
    6.  In the shell: `db.testcollection.insert({"testkey":"testvalue"})`

2.  Windows

    Refer to the Ubuntu documentation above, with the following addenda.

    ### Language Platform    

    -   Try Haskell Platform [for Windows]
        (https://www.haskell.org/platform/windows.html), or compile it from
        sources.
    -   Install [Cygwin](http://www.cygwin.com/install.html) as Cabal seems to
        need it (or an alternative called MinGW).

    ### Database

    - Install and run MongoDB [for
      Windows](http://docs.mongodb.org/manual/tutorial/install-mongodb-on-windows/).

3.  Mac

    Try Google.

### Quick Start

1.  Clone into the GitHub repository. [https://github.com/jerng/Hell.git](https://github.com/jerng/Hell.git)

    E.g.

        mkdir ~/temp
        cd ~/temp
        git clone https://github.com/jerng/Hell.git

2.  Enter your local Hell directory and resolve the local module dependencies
    with cabal-dev (which will refer to the Hell/hell-VERSION.NUMBER.cabal)

    E.g.

        cd ~/temp/Hell
        cabal-dev install

3.  Run the Hell/tryHell.sh bash script.

    E.g.

        cd ~/temp/Hell
        ./tryHell.sh

    (The script tried this - assembling then running `Hell/cabal-dev/bin/makeHell`;
    assembling then running code from `Hell/Hell` and `Hell/src` as
    `Hell/app/Server`.)

    If you see the terminal stop at the following message, then all should be well.
        
        Trying to execute ./Server (Hit Ctrl+c to kill the Server...)

4.  Navigate a web browser to [http://localhost:3000/server](http://localhost:3000/server)

    If you see any response, then you're in a good spot, for now.

## Intro

Like Yesod, Hell will pre-assemble your application's source code. Unlike
Yesod, Hell does not use Template Haskell (which compiles to Haskell AST), and
instead compiles your application into a bunch of plain Haskell scripts which
you can open up to read (read:check) if necessary. It should be noted that 
I do use a lot of libraries writted by the Yesod developers.

### Dependencies

[Haskell Platform](http://www.haskell.org/platform/) and `cabal-dev` on `Ubuntu
12.04` is the current development platform, so I'm not sure if this will work
elsewhere.

### Framework 

`./Hell` contains libraries, and some templates.
`./src` would be where you write your application code.

### Example

The only thing to look at right now, is `./src/c/Default.hs`, which is the 
Default controller, containing the (index) action. This corresponds to 
`./src/v/Default/index.hs.view`

## Status

2013-11-09 :

Finally settled the cafe client, and now I have time to get back to this
project. I cloned into the repository, in order to debug the Quick Start
process.

2013-04-18 : 

Quite a bit has been done since 2013-03-20. We've now got a controller-view
data barrier, templating, cookies, sessions, debugging to views, static file
service, redirection, and some nifty operators. We're also well on our way to
implementing BSON as the nested map type, and soon we'll integrate the `Report 
{ data_ :: Document }` field as the common data structure from models, through
to views (just like CakePHP's `$this->data`).

So far, I've tried to minimise line noise (the graphical music of morphemes, if
you will), key strokes, and typing distance. For example:

- The scripts `makeHell.hs` and `runHell.hs` are intentionally lower-cased at
  their beginnings, to save you a keystroke when `runghc whatveer.hs`-ing.
- The `(...)` operator works just like `($)`, except that its precedence is a
  notch lower, so that instead of `(a.b $ d)` you may write `(a.b...c)` - there
  are other idioms that seem to arise, but even I haven't mastered them 
  thoroughly. 

Some other design decisions:

- `Hell.Server` is structured as a pipeline. The chimerical data type that's
  passed through the pipeline is `Report`. Prior to Views, debugging stuff to
  the View is an operation like `"something to debug" ?>> report` or `report
  <<? "some text" `
- `Route`s are of the form `("controller","action")`. The (`report --> route`)
  operation redirects the client... it's a bit of an experiment, and I'm
  wondering how many other fundamental HTTP transformations we can iconify as
  graphical operators in a sensible way.
- In pipelining (composing) functions with the `(.)` operator, we're forced to
  read our function names backwards, from right to left. Sorry, my Hebrew is
  rusty, and I'd prefer not to break English, so `(>.)` mirrors `(.)`, and
  `(>...)` mirrors `(...)` so that instead of `show . concat ... [1,2,3]` we
  can instead write `[1,2,3] >... concat >. show`, which being more
  jQuery-esque, may be something that the general web development audience
  might enjoy. Well, at least I like reading code like this, some of the time.

We're far from done. It's always fun learning a new system, and I've learnt a 
fair bit of Haskellian lore just by getting the project this far. At least from
my point of view, I feel that I've learnt how to make the language, and web
development on the platform, a lot easier/prettier. So if I get blown up,
blinded, or face-chewed-off-by-a-chimp tomorrow, I guess this would be my last
happy memory pre-incident. Otherwise, I'm looking forward to further updates.

2013-03-23 (or thereabouts): 

This project is in its embryonic stages.  It remains muchly lacking in
documentation, and for that I am sorry.

What has been accomplished so far, is a Warp server application that routes
requests based on HTTP path segments. The server calls the router, the router
calls the controller-action, the controller-action passes a reaction back to
the server, the server renders the reaction into the respective view, and
serves this as a response. 


## Background

This is the first GitHub-ed version of a web application development framework
that I've been working since 2013 Feb 20. This was developed on Haskell
Platform, with a few additional packages downloaded via Cabal. I haven't learnt
how to upload anything to Cabal yet.

I'm a Haskell noob, and started working in this language in 3Q2012. I've some
previous experience with CakePHP, and have also attempted to write a similar
web development framework, called EZ, in Erlang. I've only briefly looked at
the dominant web application development frameworks for Haskell, but found them
a little too opaque for my beginner's mind, and decided to write my own, partly
for personal use, and partly as a medium for studying Haskell.

I'm also a Git noob, and will probably make mistakes with regards to licensing
this source code along the way. It has no stated license at the moment.

Please feel free to send me comments and suggestions using the channels which
GitHub provides.

### Name of the Framework

I was originally calling it it DumbFuck, censored it to DF, then figured Hell
was as good as Rails, and heck, if I have to dress it up some more for launch,
we can call it Halo. 

