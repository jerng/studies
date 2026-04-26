/*  A supervisor of subroutines (FUNCTIONS)
 *  (server/ container/ composition root/ dependency injector
 *      policy enforcement point (PEP) )
 *  
 *  CAVEAT : TRADEOFF between FLEXIBILITY and SECURITY 
 *      -   Wherever callers and callees check each other, a prejudice is
 *          introduced : INCREASING SECURITY and DECREASING FLEXIBILITY
 *      -   Classically, a mediating layer which buffers complexity is
 *          called an INTERFACE, meaning that checking does not
 *          specify a [ specific caller or callee module ] but rather a
 *          [ general pattern which the respective caller or callee
 *          module must comply with ]. Interfaces may be defined in
 *          terms of function ( module ) signatures, or in even greater
 *          specificity.
 *      -   Below, wherever there is a reference to [ caller functions ]
 *          and [ callee functions ], the reference should be read in a
 *          literally conservative, but interpretatively flexible sense,
 *          such that SHOULD THE PROGRAMMER PREFER IT, security may be
 *          relaxed, and instead of checking for [ specific functions ]
 *          the caller or callee may be checked against a [ more general
 *          interface / signature ].
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

/*  A SUPERVISOR definition :
 * */          
class Sr () {}
