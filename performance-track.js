'use strict'

    // as an AWS Lambda, nodejs function

exports.handler = async (event, context) => {

    //  Init
    //      Keys: rss, heapTotal, heapUsed, external
    //      Billed memory seems to include 35-40MB over ['rss'].

    const memoryUsageKey = 'rss'
    const { performance } = require('perf_hooks')
    const padLength = 5

    let lastMark = performance.now()
    let tempMark
    let lastMem = process.memoryUsage()[memoryUsageKey]
    let tempMem

    const rate = taskLabel => { console.log ( 
        `ms:(`,
        (Math.round  ((tempMark = performance.now()) - lastMark))
            .toString().padStart(padLength,` `), 
        `,`, 
        (Math.round  (lastMark = tempMark)).toString().padStart(padLength,` `), 
        `) ~MB:(`, 
        (Math.round  ((  
            (tempMem = process.memoryUsage()[memoryUsageKey]) - lastMem)
            /Math.pow(1024,2) * 100) / 100)
            .toString().padStart(padLength,` `), 
        `,`, 
        (Math.round  (   
            (lastMem = tempMem)
            /Math.pow(1024,2) * 100) / 100)
            .toString().padStart(padLength,` `),
        `)`,
        taskLabel
    ) }
    
    // Do stuff1
    rate ( `report on stuff 1` )

    // Do stuff2
    rate ( `report on stuff 2` )

    // Do stuff3
    rate ( `report on stuff 3` )

}

/* Example Output:

INFO	ms:(     0 , 563895 ) ~MB:(     0 ,  49.3 ) (perf_hooks) LOADED
INFO	ms:(    10 , 563905 ) ~MB:(     0 ,  49.3 ) AWS-SDK LOADED
INFO	ms:(     1 , 563906 ) ~MB:(     0 ,  49.3 ) OBJECTS CREATED
INFO	ms:(   417 , 564323 ) ~MB:(  1.02 , 50.32 ) listTables COMPLETED
INFO	ms:(    64 , 564386 ) ~MB:(     0 , 50.32 ) describeTable COMPLETED
INFO	ms:(    80 , 564466 ) ~MB:(     0 , 50.32 ) put COMPLETED
INFO	ms:(    59 , 564525 ) ~MB:(     0 , 50.32 ) get COMPLETED
INFO	ms:(    50 , 564575 ) ~MB:(     0 , 50.32 ) delete COMPLETED
INFO	ms:(    10 , 564585 ) ~MB:(     0 , 50.32 ) (fs, util) LOADED
INFO	ms:(     0 , 564586 ) ~MB:(     0 , 50.32 ) fs PROMISIFIED
INFO	ms:(     0 , 564586 ) ~MB:(     0 , 50.32 ) index.html READ

*/
