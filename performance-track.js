'use strict'

// The following code is written for the `nodejs` (12.x) runtime on AWS Lambda.
// FWIW: `nodejs.process` also has: .resourceUsage() and .httime.bigint()

//  Measuring wallclock run time.
const { performance } = require('perf_hooks')
let lastTime = performance.now()
    // There exists a similar Web API
let tempTime
let dTime

//  Measuring RAM usage.
//      `node.process.memoryUsage` keys: rss, heapTotal, heapUsed, external
//      AWS Lambda: Billed memory seems to include 35-40MB over ['rss'].
const memoryUsageKey = 'rss'
const padLength = 5
let lastMem = process.memoryUsage()[memoryUsageKey]
    // There exists a similar Web API
let tempMem

//  Measuring CPU time consumed (a subset of wallclock time).
let tempCPU = process.cpuUsage()
    // There exists NO similar Web API
let lastCPUsum = tempCPU.user + tempCPU.system
let tempCPUsum
let dCPUsum

//  So long as Lambdas are kept warm, variables declared outside the following
//      `module.exports` will not be reexecuted; only the body of the exported 
//      handler will be run on each invocation of Lambda.
module.exports = ( taskLabel, printColumnHeaders ) => { console.log ( 
    

    
    
    
    //  dRUN : stage-to-stage difference in wallclock time in milliseconds;
    //  tRUN : to-stage total wallclock time in milliseconds;
    `ms(dRUN,tRUN):(`,
    
    // delta of runtime;
    ( Math.round ( dTime = ( tempTime = performance.now() ) - lastTime ) 
    ).toString().padStart ( padLength, ` ` ), 
    `,`, 
    
    // total runtime;
    ( Math.round (lastTime = tempTime)
    ).toString().padStart ( padLength, ` ` ), 





    //  d : stage-to-stage difference in memory (whichever  metric you keyed);
    //  t : to-stage total memory consumed for (whichever metric you keyed );
    `) MB(d,t):(`, 
    
    // delta of RAM usage;
    (   Math.round (                // decimal point formatting;
    
            ( ( tempMem = process.memoryUsage()[memoryUsageKey] ) - lastMem )
            
            / Math.pow( 1024, 2 )   // B to MB conversion;
            * 100) / 100            // decimal point formatting;
    )
    .toString().padStart ( padLength, ` ` ), 
    `,`, 
    
    // total RAM usage;
    (   Math.round  (               // decimal point formatting;
    
            ( lastMem = tempMem )
            
            / Math.pow ( 1024, 2 )  // B to MB conversion;
            * 100) / 100            // decimal point formatting;
    ).toString().padStart ( padLength, ` ` ),
    
    
    
    
    
    //  dCPU : stage-to-stage difference in CPU time consumed;
    //  tCPU : to-stage total CPU time consumed;
    `) ms(dCPU,tCPU):(`,
    
    // delta of CPU time consumed;
    Math.round (
        dCPUsum =   (   (   tempCPU = process.cpuUsage(), 
                            tempCPUsum = tempCPU.user + tempCPU.system
                        )
                        - lastCPUsum 
                    )
                    / 1000          // microsecond to millisecond conversion;
    ).toString().padStart ( padLength, ` ` ),
    `,`, 
    
    // total CPU time consumed;
    Math.round (
        ( lastCPUsum = tempCPUsum ) 
        / 1000                      // microsecond to millisecond conversion;
    ).toString().padStart( padLength, ` ` ), 





    `) ms/ms(dCPU/dRUN):(`,

    //  ( delta of CPU time consumed / delta of runtime ); 
    //  absolute unit of computation;
    Math.round(
        dCPUsum / dTime 
        * 1000                      // microsecond to millisecond conversion;
    ).toString().padStart ( padLength, ` ` ), 





    `) us/ms(tCPU/tRUN):(`,

    //  ( total CPU time consumed / total runtime ); 
    //  AWS Lambda's fractional CPU allocation, based on max memory limit;
    //
    //  for example, a function allocated       128     ( MB of RAM ),
    //      is correspondingly allocated        0.125   ( vCPUs ),
    //                                      ==  80      ( us of CPU time, per 
    //                                                    ms of runtime );
    Math.round(
        lastCPUsum / lastTime 
    ).toString().padStart ( 3, ` ` ), 
    
    
    
    
    
    `)`,
    taskLabel
) }
