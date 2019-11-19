//  WARNING:    reimplement without __proto__
//              https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/proto
//
//                  Like this:
//              https://developer.mozilla.org/en-US/docs/Web/JavaScript/Inheritance_and_the_prototype_chain

export { Actor, Postman, Datum, DataModel }

console.log('jsf.js side effect')

/*  This is intended for use as a 'module'.
*       https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules
*   
*       That means we'll see the 'export' keyword prefixing many declarations.
*
*   All modules are run in 'strict mode'.
*       https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode
*
*
*
*/


// TODO: Implement error handling  via Promise chains

// TODO: EventTarget.dispatchEvent() is supposed to run listeners synchronously. Test to see if asynchronous handoff can be achieved.

/* PREP: Review overall architecture in light of this patterning concern:
*
*          Moreover, a constructor should only create and initialize a new
*          instance. It should set up data structures and all instance-specific
*          properties, but not execute any tasks. It should be a pure function
*          without side effects if possible, with all the benefits that has.
*          
*          and
* 
*          https://stackoverflow.com/questions/43431550/async-await-class-constructor
*/

//  https://developer.mozilla.org/en-US/docs/Web/API/EventTarget
//      [ class, extends, super, constructor ] keywords are only syntactic 
//      sugar  (ES6) for prototype inheritence (see ES5)
class Actor extends EventTarget {

    // (new Actor).constructor
    constructor (identity, options) {
        super ()

/* PREP:
        this.addEventListener ( 'beforeConstruction', this.beforeConstruction, { once : false } )
        this.addEventListener ( 'construction', this.construction, { once : false } )
        this.addEventListener ( 'afterConstruction', this.afterConstruction, { once : false } )

        this.dispatchEvent ( new CustomEvent ('beforeConstruction', { detail: { identity  : identity } }  ) )
        this.dispatchEvent ( new CustomEvent ('construction' ) )
        this.dispatchEvent ( new CustomEvent ('afterConstruction' ) )
*/

        this.beforeConstruction (identity)
        this.construction (options)
        this.afterConstruction ()
    }    

    // (new Actor).beforeConstruction
    beforeConstruction (identity) {
// PREP:        beforeConstruction (event) {

        if ( (typeof identity !== 'string') || (identity.length == 0) ) {
// PREP:    if ( (typeof event.detail.identity !== 'string') || (event.detail.identity.length == 0) ) {

            throw new Error (`
                (new Actor).constructor has been interrupted. Please provide 
                the instance of Actor with a .identity which 
                is a string of length > 0.`)
        } 

        this.identity                   = identity
        this.toString                   = 
            () => `Instance of Actor, identified as ${this.identity}`
// PREP:            this.identity                   = event.detail.identity

        this.actorRegistryValidation    = this.validateActorRegistry()
        this.registerActor ( this.actorRegistryValidation )
    }

    // (new Actor).construction
    construction () {
        this.inbox                  = [ ] 
        // consider moving this to the Postman class
    }

    // (new Actor).afterConstruction
    afterConstruction () {
        console.log (`NEWS: 
            An instance of ${this.constructor.name} has been constructed, identified as 
            ${this.identity}.`)
    }

    // (new Actor).validateActorRegistry
    validateActorRegistry () {
        return  {   'actorRegistryIsArray' : 

                        'actorRegistry' in window 
                        && 
                        Array.isArray(window.actorRegistry) 
                }
    }

    // (new Actor).registerActor
    registerActor (validation) {
        if ( validation.actorRegistryIsArray ) {
            window.actorRegistry.push ( this )
            console.log(`NEWS: 
                window.actorRegistry array found; pushed in an 
                instance of Actor.`)
        } else {
            window.actorRegistry    = [ this ] 
            console.log(`NEWS:
                window.actorRegistry array not found; created it, 
                containing an instance of Actor.`)
        }
    }

    // (new Actor).receiveMessage
    receiveMessage (message) {
        this.inbox.push (message)
        console.log (`NEWS:
            An actor ${this.identity} received a message: ${message}, 
            which has been pushed into ${this.identity}\'s inbox. Custom
            events are dispatched when messages arrive at inboxes, based on
            message subject.`)

        this.dispatchEvent(new CustomEvent (
            message[this.init.subjectKey], 
            { detail : { 
                content : message[this.init.contentKey],
                subject : message[this.init.subjectKey]
            } }
        ) )

    }

    // (new Actor).sendMessage
    sendMessage ( recipient, subject, content ) { }

} // end class Actor



class Postman extends Actor {

    // (new Postman).constructor
    constructor (identity) {
        super(identity)
    }

    // (new Postman).beforeConstruction
    beforeConstruction (identity) {
        super.beforeConstruction(identity)

    }

    // (new Postman).construction
    construction () {
        super.construction()

        //  super.__proto__.__proto__ is the anonymous prototoype of the 
        //      Actor class,
        //      so any assignments to that, will be inherited by all actors.
        //  CONSIDER: aliasing this to a shorter variable.
        //  CONSIDER: switching this to use Symbol.for() and a global
        //              registry instead
        //
        //  Initialised data is deleted when the script cleans up. 
        //      This helps to reduce accidental contamination of objects i
        //      via misused keys.
        super.__proto__.__proto__.init = {
            recipientKey :  Symbol('rk'),
            subjectKey   :  Symbol('sk'),
            contentKey   :  Symbol('ck'),
        }

        this.requiredKeys       = [
            this.init.recipientKey, 
            this.init.subjectKey, 
            this.init.contentKey
        ]

        // TODO: the structure of the message drawer will depend on whether
        // messaging is REST-ful, etc.
        this.messageDrawer      = [ ]

        this.recipientRegistry  = { }

    }

    // (new Postman).afterConstruction
    afterConstruction () {
        super.afterConstruction()

        console.log (`NEWS: 
            ... the new actor identified as ${this.identity}, is also 
            an instance of Postman.`)

        // Registers existing actors as recipients
        window.actorRegistry.forEach( actor => {
                if ( ! (actor instanceof Postman) ) {
                    this.registerRecipient(actor)
                }
            } )
        
        setFutureActors: {
            //  Registers future new instances of Actor as recipients
            //
            //  This sets the prototype of all Actors with new behaviours upon
            //      afterConstruction.
            //
            //  Not clear if TACTIC001 is legit.
            //
            //  TACTIC001: step 1
            var functionToOverwrite = super.afterConstruction

            super.__proto__.__proto__.afterConstruction = function(){

                //  TACTIC001: step 2
                functionToOverwrite.bind(this)()

                var postman = window.actorRegistry.find( actor => actor instanceof Postman )
                if ( typeof postman != 'undefined') {
                    postman.registerRecipient(this)
                }
            }
        }

        // Points all past and future actors to this postman
        super.__proto__.__proto__.localPostman  = this
        super.__proto__.__proto__.sendMessage   = 
            ( recipient, subject, content ) => {
                let message = { }
                message[this.init.recipientKey]    
                    = recipient
                message[this.init.subjectKey]
                    = subject
                message[this.init.contentKey]
                    = content
                 
                this.receiveMessage( message ) 
            }
    }
    
    // (new Postman).validateActorRegistry
    validateActorRegistry () {
        try {

            // TODO: consider changing this to a proper OOP-style singleton
            //      https://stackoverflow.com/questions/1479319/simplest-cleanest-way-to-implement-singleton-in-javascript
            var selfValidity

            var superValidity = super
                                .validateActorRegistry()
                                .actorRegistryIsArray

            if (    superValidity 
                    && 
                    !   (   selfValidity 
                            = ! 
                            window.actorRegistry.some ( 
                            actor => actor.constructor.name == 'Postman' 
                            ) 
                        )
               ) {
                    throw new Error (`
                        An instance of Postman pre-exists in 
                        window.actorRegistry; no other shall be 
                        registered.` )
            }
            return  {   'actorRegistryIsArray'      : superValidity,    
                        'noPreRegisteredPostman'    : selfValidity
                    }

            /*  This will return EITHER,

                    {   'actorRegistryIsArray'      : true,
                        'noPreRegisteredPostman'    : false }
                OR,
                    {   'actorRegistryIsArray'      : false,
                        'noPreRegisteredPostman'    : undefined }  */

        } catch (error) {

            console.log(error) // merely warn the developer
            return  {   'actorRegistryIsArray'      : superValidity,    // true
                        'noPreRegisteredPostman'    : selfValidity      // true
                    }
        }
    }

    // (new Postman).registerActor
    registerActor (validation) {
        if ( validation.noPreRegisteredPostman ) {
            super.registerActor(validation)
        } else {

            // TODO: consider changing this to a proper OOP-style singleton
            //      https://stackoverflow.com/questions/1479319/simplest-cleanest-way-to-implement-singleton-in-javascript
            throw new Error (`
                (new Postman).constructor has been interrupted. An instance 
                of Postman, identified as 
                ${this.identity}, may not be constructed.`)
        }
    }

    // (new Postman).registerRecipient
    registerRecipient (actor) {
        try {


            if ( ! actor instanceof Actor  ) {
                throw `Warning:
                    An instance of Postman tried to register an invalid 
                    recipient: ${actor} (an instance of Actor is expected)`
            }

            if ( actor.identity in this.recipientRegistry  ) {
                throw `Warning:
                An instance of Postman may not register a recipient 
                whose identity is already in the Postman's registry: 
                ${actor.identity}.`
            }

            this.recipientRegistry[actor.identity] = actor
            console.log (`NEWS: 
                An instance of Postman registered a new recipient: 
                ${actor.identity}`)

            return true

        } catch (error) {

            console.error(error)

            return false
        }
    }

    // (new Postman).receiveMessage
    receiveMessage (message) {
        if ( ! this.validateMessage (message) ) {
            return false
        }

        if ( typeof
            this.recipientRegistry[message[super.__proto__.__proto__.init.recipientKey]]
            == 'undefined' ) {

            this.messageDrawer.push (message)

            console.log (`Warning:
                Postman received a message for an unregistered
                recipient,
                ${message[super.__proto__.__proto__.init.recipientKey]},
                it will be kept in the drawer.`)

            return false

        }

        this
            .recipientRegistry[message[super.__proto__.__proto__.init.recipientKey]]
            .receiveMessage (message)

        return true
    }
    
    // (new Postman).validateMessage
    validateMessage (message) {
        try {

            if ( typeof message !== 'object'  ) {
                throw `Warning:
                    An instance of Postman received an invalid message: 
                    ${message} (an object is expected)`
            }

            var keys = Object.getOwnPropertySymbols (message)

            var allRequiredKeysFound =  this.requiredKeys.every ( 
                                            key => keys.includes(key) 
                                        )

            if  ( ! allRequiredKeysFound ) {
                throw `Warning:
                    An instance of Postman received an invalid message: 
                    ${message} (required keys were missing)`
            }

            return true

        } catch (error) {

            console.error(error)

            return false
        }
    }

} // end class Postman



class Datum extends Actor {

    // (new Datum).constructor
    constructor (identity) {
        super (identity)
    }

    // (new Datum).beforeConstruction
    beforeConstruction (identity) {
        super.beforeConstruction (identity)

        this.datumRegistryValidation = this.validateDatumRegistry()
        this.registerDatum ( this.datumRegistryValidation )
    }

    // (new Datum).construction
    construction () {
        super.construction ()


        // TODO: consider if the following properties should be not-enumerable

        this.cache      =   {
            'hit'   : false,
            'value' : undefined
        }
        
        this.dependencies   =   [   ]   
            //  entries should be 
            //  { 'id' : String, 'cache' : { 'hit' : Boolean, 'value' : someValue }
            //
            //  1.  Perhaps alias this to this.providers
            //  2.  Perhaps create a Proxy?getter that grabs the provider's
            //  Datum's value directly.
        this.dependencies.toString = () => this.dependencies.reduce(
            (acc,cur,idx,src) => `${acc}  '${cur.id}'`, ''
        )
        // TODO: consider generalising this pattern to the other functions below


        this.dependents     =   [   ]
            //  entries should be { 'id' : String } for consistency

        this.viewNodes      =   [   ]
            //  entries should be { 'id' : String } for consistency

        this.evaluation =   () => undefined

    }

    // (new Datum).afterConstruction
    afterConstruction () {
        console.log (`NEWS: 
            An instance of Datum has been constructed, identified as 
            ${this.identity}.`)
    }

    // (new Datum).validateDatumRegistry
    validateDatumRegistry () {
        return  {   'datumRegistryIsMap' : 

                        'datumRegistry' in window 
                        && 
                        window.datumRegistry instanceof Map
                }
    }

    // (new Datum).registerDatum
    registerDatum (validation) {
        if ( validation.datumRegistryIsMap ) {
            window.datumRegistry.set ( this.identity, this )
            console.log(`NEWS: 
                window.datumRegistry map found; set an 
                instance of Datum, using its identity as the key.`)
        } else {
            window.datumRegistry    = new Map ( [ [this.identity, this] ] ) 
            console.log(`NEWS:
                window.datumRegistry datum not found; created it, 
                containing an instance of Datum, with its identity as the key.`)
        }
    }



} // end class Datum


class DataModel extends Actor {
    
    // (new DataModel).constructor
    constructor (identity, options) {
        super (identity, options)
    }

    // (new DataModel).beforeConstruction
    beforeConstruction (identity) {
        super.beforeConstruction (identity)
    }

    // (new DataModel).construction
    construction (options) {
        super.construction (options)

        var _global = options.global

        var handler = {

            set : function ( targ, prop, val, rcvr ) {

            },

            get : function ( targ, prop, rcvr ) {

                // TODO: rewrite as if (prop in list) { return targ[prop]} ?
                switch (prop) {

                    case  'sendMessage' :
                        return targ.sendMessage
                        break 
                    
                    default:
                }

                if ( ! _global.datumRegistry.has (prop) ) {
                    throw new Error (`An instance of DataModel was called with
                    the property '${prop}', however no instance of Datum
                    identified as such was found in the global datumRegistry.`)
                } 

//* TODO: this should move into Datum logic, by the next commit
{
                var datum = _global.datumRegistry.get (prop)
                if ( ! datum.cache.hit ) {
                    datum.cache.value   = _global.datumRegistry.get(prop).evaluation() 
                    //  datum.evaluation() is supposed to return the recomputed
                    //  value of (datum). But if it hits an error, what happens?
                    //  - It does not return - should this be handled by 'async'
                    //      code?   (Currently: code will just block())
                    //  - It encounters an error - should this be handled by
                    //      'try'? (Currently: code will just halt.)
                    //  TODO: (definitely before releasing v1.0)
                    //

                    datum.cache.hit     = true
                }

                var result = datum.cache.value

                // targets a Datum
                // checks if a there are dependencies                    
                //
 }
 //*/               

                return result 
            },

        }

        _global.$$ = new Proxy (this, handler)  
        // TODO: consider disabling reassignment; 
        //      also consider disabling reassignment for other globals

    }

    // (new DataModel).afterConstruction
    afterConstruction () {
        super.afterConstruction()
    }
    
    
} // end class DataModel
