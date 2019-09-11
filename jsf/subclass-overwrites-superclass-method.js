//  super.__proto__.__proto__ is the anonymous prototoype of the 
//      Actor class,
//      so any assignments to that, will be inherited by all actors.






// Postman is a subclass of Actor, and it has a method:





        // (new Postman).afterConstruction
        afterConstruction () {
            super.afterConstruction()

            console.log (`NEWS: 
                ... the new actor identified as ${this.identity}, is also 
                an instance of Postman.`)

            window.actorRegistry.forEach( actor =>{
                    if ( ! (actor instanceof Postman) ) {
                        this.registerRecipient(actor)
                    }
                } )

            //  This sets the prototype of all Actors with new behaviours upon
            //      afterConstruction.
            //
            //  Not clear if TACTIC001 is legit.

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
