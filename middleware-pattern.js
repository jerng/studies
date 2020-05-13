'use strict'

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
            ( fulfillFn, rejectFn ) => {}   // an `executor`
            
        ).then(
            onFulfilled ( value )   => {},  // a Promise is `settled`1
            onRejected  ( reason )  => {}   // a Promise is `settled`2
            
        ).catch(                            // sugared  .then()
            onRejected  ( reason )  => {}
            
        ).finally(
            regardless  ()          => {}   //  No argument is ever passed.
        )
    ) 
    
-   Do NOT use:     `resolve`   in place of     `fulfill`
                    `resolved`  in place of     `settled`
                    `result`    in place of     `value`
                    `error`     in place of     `reason`

    Heuristic: prefer standards (the etymology is complex; I have a slide).

-   ALWAYS enter both arguments of Executors, and .then(), EVEN IF one argument
    will not be used. For minimal line noise, consider using `_`, `f`, `r`, 
    `onF`, `onR`. `value => {}`, `reason => {}`. Heuristic: terseness; 
    explicitly deny options.

-   ALWAYS use `await`, and therefore `try { await } catch (e) { handler }`
    UNLESS some of the above is more succinct OR you don't have a wrapping 
    `async` function context. Heuristics: terseness; explicitly state 
    intentions.


*/

module.exports = async ( LAMBDA_ARGUMENTS, MIDDLEWARE_QUEUE ) => {
    
    const data = { LAMBDA: {
        
            event:      LAMBDA_ARGUMENTS[0],
            context:    LAMBDA_ARGUMENTS[1],
            callback:   LAMBDA_ARGUMENTS[2]     }   }

    return  MIDDLEWARE_QUEUE.reduce (
        
                ( DATA, CURRENT_MIDDLEWARE ) => CURRENT_MIDDLEWARE ( DATA ), 
                
                data
            )
}
const mark      = require ( 'mark' )            
mark (`ruthenium.js LOADED`, true)
