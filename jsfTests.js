//  WARNING:    reimplement without __proto__
//              https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/proto
//
//                  Like this:
//              https://developer.mozilla.org/en-US/docs/Web/JavaScript/Inheritance_and_the_prototype_chain

import { Actor, Postman, Datum, DataModel } from './jsf.js'

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
    m[pam.__proto__.__proto__.init.senderKey]       = 'jsfTests.js'
console.log ('TEST: A variable, m, has been declared with valid message keys.')

console.log ('TEST: ... we then attempt to inbox pam, the postman, with m:')
pam.receiveMessage(m)

// Test cleanup
m = null 

console.log ('TEST: ... after that, we then attempt to inbox Pam, the postman, with an empty object literal, {}:')
pam.receiveMessage({})

console.log ('TEST: ... then we attempt to register actor Joe_2 in the same.')
let joe2    = new Actor ('Joe')

console.log ('TEST: ... then we attempt to register actor Joe_3 in the same.')
let joe3    = new Actor ('Joe3')

console.log (`TEST:
    Whereas, window.actorRegistry is an array that can hold multiple 
    discrete instances of Actor which each have the same .identity: 
    ${window.actorRegistry.map( actor => actor.identity).join(', ') }`
)
console.log(`TEST: window.actorRegistry identities: ${window.actorRegistry.map(x => x.identity)}`)
console.log(`TEST: pam.recipientRegistry identities: ${Object.keys(pam.recipientRegistry)}`)

console.log(`TEST: add event listener to Joe, and get Jae to send Joe a message.`)
joe1.addEventListener ( 'bye', event => 
    console.log (`
         I, '${event.target.identity}', am triggered by the
         event, '${event.detail.subject}', sent by '${event.detail.sender}' and
         will now spew the content: '${event.detail.content}'`
    )
)
jae.sendMessage('Joe','bye','so high', jae.identity)

console.log(`TEST: Attempt to create five Datum objects...`)

new Datum ('field1')
new Datum ('field2')
new Datum ('field3')
new Datum ('field4')
new Datum ('field5')

console.log(`TEST: Attempt to specify Datums 'field2' and 'field3' as dependencies of 'field1'...`)

window.datumRegistry.get('field1').dependencies.push('field2','field3')

console.log(`TEST: Attempt to (directly) specify Datums 'field2' and 'field3'
evaluation()s to constants...`)

window.datumRegistry.get('field2').evaluation = () => `(F2_PLACEHOLDER_VALUE)`
window.datumRegistry.get('field3').evaluation = () => `(F3_PLACEHOLDER_VALUE)`

console.log(`TEST: ... now whats in window.datumRegistry?`)

{
    let pad = '\n'
    window.datumRegistry.forEach( (v,k,m) => {

        console.log(`key: ${k}, Object.getOwnPropertyNames(value):
            ${  Object.getOwnPropertyNames(v).reduce( 
                    (acc,cur,idx,src) => `${acc}  '${cur}'` + pad, pad
            ) }`
        )

        console.log(`- dependencies' IDs: 
            ${  v.dependencies.reduce( 
                    (acc,cur,idx,src) => `${acc}  '${cur.id}'` + pad, pad
            ) }`
        )


    } )
}

console.log (`TEST: 
                ... now whats each Datum's evaluation (accessed directly
                from each Datum)?`)

window.datumRegistry.forEach( 
    (v,k,m) => console.log( 
        `datumID: ${v.identity}, evaluates to: ${v.evaluation()}` 
    ) 
)













console.log (`TEST: 
                Creating a new DataModel with the 'window' object passed in a
                the global object ( any Datums previously tested should
                henceforth behave differently...) ...`)

new DataModel ( 'store', {'global':window} )

try             {   console.log (`TEST: $$.undefinedfield: ${$$.undefinedfield}`)  } 
catch (error)   {   console.error (error); console.error (`Execution would normally
halt here, but this test script has been written to continue.`) }

console.log (`TEST: $$.field2: ${$$.field2}`)

console.log(`TEST: Attempt to (directly) specify Datums 'field1' as computed 
evaluation()s based on the dependencies 'field2' and 'field3'...`)

window.datumRegistry.get('field1').evaluation = () => `[${$$.field2 + $$.field3}]`

console.log (`TEST: 
                ... now whats each Datum's evaluation (accessed directly
                from each Datum)?`)

window.datumRegistry.forEach( 
    (v,k,m) => console.log( 
        `datumID: ${v.identity}, evaluates to: ${v.evaluation()}` 
    ) 
)

console.log(`TEST: window.actorRegistry identities: ${window.actorRegistry.map(x => x.identity)}`)
console.log(`TEST: pam.recipientRegistry identities: ${Object.keys(pam.recipientRegistry)}`)

console.log(`
    DEMO: Here's what you can now do. You can access an already declared
    instance of DataModel, via the global variable $$. This variable points to
    an instance of Proxy over the DataModel. 

    You may create, update, and read (but not yet delete) properties of
    DataModel e.g.:

    >>> $$.newProp = 1  // create
    >>> $$.newProp      // read : 1
    >>> $$.newProp = 2  // update 
    >>> $$.newProp      // read : 2

    You may also assign computed values to DataModel props, in the following
    manner:

    >>> $$.newProp1 = 1         //  create
    >>> $$.newProp2 = 2         //  create

    >>> $$.newProp3 = () => $$.newProp1 + $$.newProp2
            //  create
    >>> $$.setDependencies('newProp3', ['newProp1', 'newProp2'])   
            // clunky registration, of dependencies AND dependents

    >>> $$.newProp3             // read : 3
    >>> $$.newProp2 = 4         // update
    >>> $$.newProp3             // read : 5

`)

console.log(`
    development paused at class DataModel; we are going to finish making this
    framework on synchronous models because despatchEvent inherently prevents us
    from using asynchronous code in event listeners`)

console.log ('// TESTS //\n// TESTS // Let\'s do some simple tests.\n// END //')



//*//




//  TODO: can't do this yet, as we haven't figured out how to send messages
//  without it.
//
//      delete pam.__proto__.__proto__.init
//
//      console.log ('TEST: After script cleanups deleted pam.__proto__.__proto__.init, pam.init now has the value of: ' + pam.init ) 

//  TODO:    class ViewNode extends Actor { }

//  Consideration 2:
//  
//      2.1.    Learnt about Object.defineProperty just in time to include it:
//      https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
//
//      2.2.    Learnt about Proxy, Reflect.
//

//  The model compiler should read the dataModel, and write the
//  dataImplementation. 
//
//     After creation of each Datum in the       datastore, dependencies can
//     be       implemented between each Datum and       the other Datums.
//     If lazy evaluation       is permitted, circular dependencies
//     could be enabled with JavaScript       generators.  
//      
