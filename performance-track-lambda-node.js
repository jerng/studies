'use strict'

// The following code is written for the `nodejs` (12.x) runtime on AWS Lambda.
// FWIW: `nodejs.process` also has: .resourceUsage() and .httime.bigint()

//  `INVOCATION` below refers to invocation of the LAMBDA FUNCTION

///////////////////////////////////////////////////////////////////////////////

/*  EXAMPLE OF USE: 
 *
   'use strict'
    const L = require ( './libraries.js')
    
    
    exports.handler = async (event, context) =>  {
        const startCPU = L.process.cpuUsage()
        const startValues = {
            startTime : L.performance.now(),
            startCPUsum : startCPU.system + startCPU.user
        }
        
        L.mark ( `handler BEGAN`, startValues, true )
            // FIRST CALL MUST SET THIRD PARAMETER TO TRUE
            
        const AWS = require ( 'aws-sdk' )
        
        L.mark ( `AWS-SDK LOADED`, startValues )
            // SUBSEQUENT CALLS MUST SET THIRD PARAMETER TO FALSY

*
*/



//
//
//
//

let newExecutionContext = true
let firstInvocation = true
let nthInvocation
let preInvocationTime
let preInvocationCPUsum

//  Measuring wallclock run time.
const { performance } = require('perf_hooks')
        // There exists a similar Web API
const initTime = performance.now()
        // This will only run once per execution context (EC);
        // multiple function invocations may occur within the same EC;
        // periodically, the Lambda service will destroy the EC;
        // a function invocation after that must create a new EC. 
let lastTime

//
//
//
//

//  Measuring RAM usage.
//      `node.process.memoryUsage` keys: rss, heapTotal, heapUsed, external
//      AWS Lambda: Billed memory seems to include 35-40MB over ['rss'].
const memoryUsageKey = 'rss'
const padLength = 9
let lastMem = process.memoryUsage()[memoryUsageKey]
    // There exists a similar Web API

//
//
//
//

//  Measuring CPU time consumed (a subset of wallclock time).
    // Using `process.cpuUsage`
    // There exists NO similar Web API
const initCPU = process.cpuUsage()
        // This will only run once per execution context (EC);
        // multiple function invocations may occur within the same EC;
        // periodically, the Lambda service will destroy the EC;
        // a function invocation after that must create a new EC. 
const initCPUsum = initCPU.user + initCPU.system
let lastCPUsum

//
//
//
//
const mark = ( taskLabel, startValues, firstInHandler ) => {

    if ( newExecutionContext ) {
        if ( firstInHandler ) {
            const preInvocationCPU = process.cpuUsage()
            preInvocationCPUsum =   preInvocationCPU.user 
                                    + preInvocationCPU.system
            preInvocationTime = performance.now()
            newExecutionContext = false
            console.log (
                
                String(`preInvoke: ` + Math.round(preInvocationTime) + ` ms`)
                    .padStart ( 2 * padLength + 4, ` `) , 

                String(`"${memoryUsageKey}"`)
                    .padStart ( 2 * padLength + 6, ` `) , 
                
                String(`preInvoke: ` + Math.round(preInvocationCPUsum/1000) + ` ms`)
                    .padStart ( 2 * padLength + 4, ` `) , 

                String(`throttle ⚠️ ㇏㇏`)
                    .padStart ( 2 * padLength + 6, ` `)
            )
            console.log (
                String('').padEnd ( 26 + 8 * padLength, `-` )
            )
            
        } else {
            throw   `perf.js; mark(_,_,firstInHandler); firstInHandler needs to 
                    be set to true, if this is the first time you are calling 
                    (mark); you can view an example in the commetns of perf.js`
        }
    } 
    else 
    if ( firstInvocation && firstInHandler ) { firstInvocation = false }
        //  The second time the execution context runs into 
        //  (firstInHandler == true), it tell itself this is no longer the 
        //  first invocation; now we have a way to flag the first invocation 
        //  for all calls to (mark).

    if ( firstInHandler ) {
        nthInvocation = 0
        console.log (
            String(`WALL-🕓:(Δ,Σ)`)
                .padStart ( 5 + 2 * padLength, ` ` ),
            String(`RAM:(Δ,Σ)`)
                .padStart ( 5 + 2 * padLength, ` ` ),
            String(`CPU-🕓:(Δ,Σ)`)
                .padStart ( 5 + 2 * padLength, ` ` ),
            String(`[CPU-🕓/WALL-🕓]:(Δ,Σ)`)
                .padStart ( 8 + 2 * padLength, ` ` )
        )
        console.log (
            String('').padEnd ( 26 + 8 * padLength, `-` )
        )
    }
    nthInvocation ++

    if (    ( ! startValues ) 
        ||  ( ! startValues.startTime )
        ||  ( ! startValues.startCPUsum )
    ) { 
        throw   `perf.js; mark(_,startValues,_); startValues needs to have the 
                (startTime) and (startCPUsum) values; you can view an example 
                in the comments of perf.js`
    }

    lastTime = firstInHandler 
        ? 0 
        : lastTime 
    let tempTime
    let dTime
    
    lastCPUsum = firstInHandler 
        ? 0 
        : lastCPUsum 
    let tempCPU   
    let tempCPUsum
    let dCPUsum

    let tempMem
    
    console.log ( 
        
    //
    //
    //
    //  (Block 1)
    
    //  dRUN : stage-to-stage difference in wallclock time in milliseconds;
    //  tRUN : to-stage total wallclock time in milliseconds;

    // delta of runtime;
    Math.round (    dTime =
                    (   tempTime = performance.now() 
                        - ( firstInvocation ? 0 : startValues.startTime ) 
                    ) - lastTime 
                 
    ).toString().padStart ( padLength, ` ` ), 

    // total runtime;
    Math.round ( lastTime = tempTime 
    ).toString().padStart ( padLength, ` ` ), 

    `ms|`,

    //
    //
    //
    //  (Block 2)

    //  d : stage-to-stage difference in memory (whichever  metric you keyed);
    //  t : to-stage total memory consumed for (whichever metric you keyed );
    
    // delta of RAM usage;
    
    (   Math.round (                // decimal point formatting;
    
            ( ( tempMem = process.memoryUsage()[memoryUsageKey] ) - lastMem )
            
            / Math.pow( 1024, 2 )   // B to MB conversion;
            * 100) / 100            // decimal point formatting;
    )
    .toString().padStart ( padLength, ` ` ), 

    // total RAM usage;
    
    (   Math.round (                // decimal point formatting;
    
            ( lastMem = tempMem )
            / Math.pow ( 1024, 2 )  // B to MB conversion;:
            * 100) / 100            // decimal point formatting;
    ).toString().padStart ( padLength, ` ` ),
    `MB|`, 
   
    //
    //
    //
    //  (Block 3)
    
    //  dCPU : stage-to-stage difference in CPU time consumed; 
    //  tCPU : to-stage total CPU time consumed;
    
    // delta of CPU time consumed;
    
    Math.round (
        ( dCPUsum = (   tempCPU = process.cpuUsage(), 
                        tempCPUsum =    
                            tempCPU.user 
                            + tempCPU.system
                            - ( firstInvocation ? 0 : startValues.startCPUsum )
                    )
                    - lastCPUsum 
                )
                / 1000              // microsecond to millisecond conversion;
    ).toString().padStart ( padLength, ` ` ),

    // total CPU time consumed;
    
    Math.round (
        (   lastCPUsum  = tempCPUsum ) 
        / 1000                      // microsecond to millisecond conversion;
    ).toString().padStart( padLength, ` ` ), 
    `ms|`, 

    //
    //
    //
    //  (Block 4)

    //  ( delta of CPU time consumed / delta of runtime ); 
    //  stage-to-stage CPU allocation; volatile; subject to long-term average;
    
    Math.round (
        dCPUsum / dTime 
    ).toString().padStart ( padLength, ` ` ), 
    
    //
    //
    //
    //  (Block 5)

    //  ( total CPU time consumed / total runtime );
    //  to-stage average CPU allocation; the long-term average;
    //  AWS Lambda's fractional CPU allocation, based on max memory limit;
    //
    //  for example, a function allocated       128     ( MB of RAM ),
    //      is correspondingly allocated        0.125   ( vCPUs ),
    //                                      ==  80      ( us of CPU time, per 
    //                                                    ms of runtime );
    
    Math.round (
        lastCPUsum / lastTime 
    ).toString().padStart ( padLength, ` ` ), 
    `µs/ms|`,
    
    //
    //
    //
    //
    
    taskLabel,
    ( nthInvocation % 3  ?  "": "\n" )
) }

//  So long as Lambdas are kept warm, variables declared outside the following
//      `module.exports` will not be reexecuted; only the body of the exported 
//      handler will be run on each invocation of Lambda.
module.exports = {
    performance : performance,
    mark : mark
}
