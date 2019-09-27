//  WARNING:    reimplement without __proto__
//              https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/proto
//
//                  Like this:
//              https://developer.mozilla.org/en-US/docs/Web/JavaScript/Inheritance_and_the_prototype_chain

import { Actor, Postman } from './jsf.js'

//*   Some simple pre-tests:

console.log ('// BEGIN //\n// TESTS // Let\'s do some simple pre-tests.\n// TESTS //')

try             { let nemo    = new Actor () } 
catch (error)   { console.error (error); console.error (`Execution would normally
halt here, but this test script has been written to continue.`) }

let joe1    = new Actor ('Joe')
let jee     = new Actor ('Jee')
let jae     = new Actor ('Jae')
let pam     = new Postman ('Pam')

try             { let pat     = new Postman ('Pat') }
catch (error)   { console.error (error); console.error (`Execution would normally
halt here, but this test script has been written to continue.`) }

let m       =  {}
    m[pam.__proto__.__proto__.init.recipientKey]    = 1
    m[pam.__proto__.__proto__.init.subjectKey]      = 2
    m[pam.__proto__.__proto__.init.contentKey]      = 3
console.log ('TEST: A variable, m, has been declared with valid message keys.')

console.log ('TEST: ... we then attempt to inbox pam, the postman, with m:')
pam.receiveMessage(m)

// Test cleanup
m = null 

console.log ('TEST: ... after that, we then attempt to inbox Pam, the postman, with an empty object literal, {}:')
pam.receiveMessage({})

console.log ('TEST: ... then we attempt to register actor Joe_2 in the same.')
let joe2    = new Actor ('Joe')

console.log (`TEST:
    Whereas, window.actorRegistry is an array that can hold multiple 
    discrete instances of Actor which each have the same .identity: 
    ${window.actorRegistry.map( actor => actor.identity).join(', ') }`
)
console.log(`TEST: window.actorRegistry identities: ${window.actorRegistry.map(x => x.identity)}`)
console.log(`TEST: pam.recipientRegistry identities: ${Object.keys(pam.recipientRegistry)}`)

console.log(`TEST: add event listener to Joe, and get Jae to send Joe a message.`)
joe1.addEventListener (
    'bye',
     event => console.log (`
         I, '${event.target.identity}', am triggered by the
         event, '${event.detail.subject}', and will now spew the content:
         '${event.detail.content}'`)
)
jae.sendMessage('Joe','bye','so high')

console.log ('// TESTS //\n// TESTS // Let\'s do some simple tests.\n// END //')

//*//




//  TODO: can't do this yet, as we haven't figured out how to send messages
//  without it.
//
//      delete pam.__proto__.__proto__.init
//
//      console.log ('TEST: After script cleanups deleted pam.__proto__.__proto__.init, pam.init now has the value of: ' + pam.init ) 




//* Building a reactive store...

    //  Consideration 1:
    class Datum extends Actor {
            
        }

    //  Consideration 2:
    //  
    //      2.1.    Learnt about Object.defineProperty just in time to include it:
    //      https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
    //
    //      2.2.    Learnt about Proxy, Reflect.
    //
    let dataModel = {               // aka 'model' : one web component, maps to one model

        metadata : {

        },        

        dataDefinition : {         

            a : {                   // each datum's key is its unique identifier (UID)
                type : Number,      // initialised or (undefined)
                value : () => 1,    // initialised or (undefined)
                neverCache : true   // initialised or (undefined)
            },

            b : {                   
                type : Number,      
                value : () => new Date,
            },

            c : {                   
                type : Number,      
                value : () => query('some syntax which refers to data UIDs'),
                    // We have to write the query function.
            }

        },


    }

    let dataImplementation = {

        datastore : {

            a : (new Datum),        // Something like this is probably going to happen

            b : (new Datum),        // You could Proxy a Datum for sophistication

            c : (new Datum),        /*  The model compiler should read the
                                     *      dataModel, and write the
                                     *      dataImplementation.
                                     *
                                     *      After creation of each Datum in the
                                     *      datastore, dependencies can be
                                     *      implemented between each Datum and
                                     *      the other Datums. If lazy evaluation
                                     *      is permitted, circular dependencies
                                     *      could be enabled with JavaScript
                                     *      generators. 
                                     */

        },
    
        cache : {                   // To invalidate a datum, delete its cached
                                    //  key.

            a : {
                value : undefined   // Initial parse of dataDefinition may
                                    //  initialise a cache.
            }

        }
    
    }

//*/
