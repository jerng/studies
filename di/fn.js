/*  A supervisor of subroutines (FUNCTIONS)
 *  (server/ container/ composition root/ dependency injector
 *      policy enforcement point (PEP) )
 *  
 *  1. Supervisor is given :
 *      1. caller function's
 *          1. id
 *      2. CALLEE function's
 *          1. id
 *          2. input arguments 
 *  
 *  2. Supervisor checks
 *      1. input arguments against CALLEE's signature
 *      2. caller id against CALLEE's prerun-allowed-caller list
 *  
 *  3. Supervisor builds TREE-of-checks based on CALLEE's descendents'
 *          prerun-allowed-lists
 *  
 *  4. Supervisor checks (all above), against policy decision point (PDP)
 *
 *  5. After passing PDP, Supervisor immediately runs CALLEE's { before,
 *      main, after, log } logic, executing runtime-allowed-callee
 *      checks as required. 
 *
 *
 * */


/* No (non-trivial) FUNCTION is called imperatively. 
 *      All calling is declarative.
 *
 * A FUNCTION definition :
 * */
{   
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

    /* CONTROL props */
    allowed : {
        access :    Function,
        prerun : {
            callers : {
                key0 :  Object, /* etc. */
            },
            callees : {
                key0 :  Object, /* etc. */
            }
        },
        runtime : {
            callees : {
                key0 :  Object, /* etc. */
            }
        }
    },

    /* LOGIC props */
    beforeMain :    Function,
    main :          Function,
    afterMain :     Function,   /* OUTPUT, must be explicitly concluded */
    logTrail :      Function,   /* final logic, before subroutine RETURNS OUTPUT */

    /* ERRORS */
    errorMode :     Number,     /* low-tolerance just breaks, high- may collate errors  */
    errors    :     [ Error ]
}
