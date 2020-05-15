'use strict'
const util      = require ( 'util' )
const reducer   = require ( 'rutheniumReducer' )

const ruthenium = async ( LAMBDA_ARGUMENTS, MIDDLEWARE_QUEUE ) => {
    
    const initialData = Promise.resolve ( { // clunky but more explicit thatn async IIFE
        
        LAMBDA: {
            
            //  Things we must include because they are principal arguments of 
            //  Lambda invocation handlers.
            event:          LAMBDA_ARGUMENTS[0],
            context:        LAMBDA_ARGUMENTS[1],
            callback:       LAMBDA_ARGUMENTS[2],
            
            //  Things which may not be immediately obvious, which we should
            //  encourage developers to be aware of.
            inspectGlobal:  () => util.inspect ( global, { 
                depth:      Infinity, 
                showHidden: true
            } )
        },
        
        RU: {
            middlewares: MIDDLEWARE_QUEUE,  // questionable/dangerous!
            errors:     [],
        }
    } )

    // reducer ( getHeaders, initialData  ), non-Promise goes in, Promise comes out
    // reducer ( getHeaders, business  ), Promise goes in

    return await MIDDLEWARE_QUEUE.reduce ( reducer , initialData )
}
module.exports = ruthenium

const mark      = require ( 'mark' )            
mark (`ruthenium.js LOADED`, true)

/*

* How this Software Framework was Named *

The letter after Lambda is Mu, which is nice and short, but it is also already
taken in the universe of software development framework names. Nu is a little
common as far as glyphs go in the English language. The letter Rho comes along a
little further on. Rhodium has the chemical shortform Rh, and its pronunciation
is not as sharp as I'd like a tool like this to be named. Ruthenium's symbol is 
Ru, and it speaks like the Mandarin Chinese character 入 (rù), "entry", which
looks like Lambda. No other software appears to be called Ruthenium, so I think 
we are all set here.

* How to Deploy Functions in this Software Framework *

-   ALWAYS use arrow function expressions (AFEs), UNLESS there is a specific 
    need to refer to a function as `this` from within its own body ... and to
    a less significant degree, if you need the function's `arguments` array.
    Heuristics: prefer terseness; explicitly state intentions.

-   ALWAYS use async functions, UNLESS there is a specific advantage to force 
    synchronous responses. Heuristic: prefer decoupling.
    
-   Use generator functions ONLY when there is a specific need for such
    functionality. (Note added for completeness. Did we miss any other type of 
    function?)

* How to Deploy Promises in this Software Framework *

-   ALWAYS use the following taxonomy:

    (   Promise ( 
            ( resolveFn, rejectFn ) => {}   // an `executor`
            
        ).then(
            onResolved  ( value )   => {},  // a Promise is `settled`1
            onRejected  ( reason )  => {}   // a Promise is `settled`2
            
        ).catch(                            // sugared  .then()
            onRejected  ( reason )  => {}
            
        ).finally(
            regardless  ()          => {}   //  No argument is ever passed.
        )
    ) 
    
-   Do NOT use:     `fulfill`   in place of     `resolve` 

                        (   while `fulfill` is more historically correct, it is 
                            unfortunately contradicted by the ECMAScript 
                            specification choice of .resolve() as the relevant
                            method with its own name; 
                        )
                        
                    `resolved`  in place of     `settled`
                    `result`    in place of     `value`
                    `error`     in place of     `reason`

    Heuristic: prefer standards (the etymology is complex; I have a slide).

-   ALWAYS enter both arguments of Executors, and .then(), EVEN IF one argument
    will not be used. For minimal line noise, consider using `_`, `res`, `rej`, 
    `onRes`, `onRej`, `value => {}`, `reason => {}`. Heuristic: terseness; 
    explicitly deny options.

-   ALWAYS use `await`, and therefore `try { await } catch (e) { handler }`
    UNLESS some of the above is more succinct OR you don't have a wrapping 
    `async` function context. Heuristics: terseness; explicitly state 
    intentions.


*/