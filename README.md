# Hell

An aspiring MVC web application development framework, for the Haskell
programming language.

## Status

This project is in its embryonic stages.  It remains muchly lacking in
documentation, and for that I am sorry.

What has been accomplished so far, is a Warp server application that routes
requests based on HTTP path segments. The server calls the router, the router
calls the controller-action, the controller-action passes a reaction back to
the server, the server renders the reaction into the respective view, and
serves this as a response. 

## Intro

Like Yesod, Hell will pre-assemble your application's source code. Unlike
Yesod, Hell does not use Template Haskell (which compiles to Haskell AST), and
instead compiles your application into a bunch of plain Haskell scripts which
you can open up to read (read:check) if necessary.

### Dependencies

[Haskell Platform](http://www.haskell.org/platform/) will suffice. If a
required module is missing, I'm afraid that, for now, you'll just have to
Google to find the respective package, and get it via `cabal install`.

### Framework 

`./Hell` contains libraries, and some templates.
`./src` would be where you write your application code.

Running `runghc ./makeHell.hs` would assemble code from the two directories
mentioned above. The assembled code would be written to `./app`.

Running `runghc ./tryHell.hs` would run `./app/Server.hs`.

### Example

The only thing to look at right now, is `./src/c/Default.hs`, which is the 
Default controller, containing the (index) action. This corresponds to 
`./src/v/Default/index.hs.view`

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
