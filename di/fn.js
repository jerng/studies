/* No (non-trivial) FUNCTION is called imperatively. 
 *      All calling is declarative.
 * */

/* A FUNCTION definition :
 * */
//const Fn = function(){} // unsweetened class declaration
class Fn {
    static  { 
        Object.assign( this.prototype, {   
            /* ABSTRACT props */
            id :            String,
            pure :          Boolean,
            idempotent :    Boolean, 
            signature : {
                in : {
                    key0 : {
                        schema : Object
                    } /* etc. */
                },
                out : { schema : Object }
            },

            /* CONTROL props ---> moved out, to Policy Information Point (PIP) */
            // allowed : { },

            /* LOGIC props */
            beforeMain :    Function,
            main :          Function,
            afterMain :     Function,   /* OUTPUT, must be explicitly concluded */
            logTrail :      Function,   /* final logic, before subroutine RETURNS OUTPUT */

            /* ERRORS */
            errorMode :     Number,     /* low-tolerance just breaks, high- may collate errors  */
            errors    :     [ Error ]
        } ) 
    }
}
console.log(JSON.stringify(Object.getPrototypeOf(new Fn),null,3))
