// As an AWS Lambda function:


exports.handler = async (event,context,callback) => {

    const create_UUID = () => 
    {
        let dt = new Date().getTime();
        let uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
            var r = (dt + Math.random()*16)%16 | 0;
            dt = Math.floor(dt/16);
            return (c=='x' ? r :(r&0x3|0x8)).toString(16);
        });
        return uuid;
    }

    let haystack = []
    for ( let counter = 0; counter < 50000; counter ++ )
    {
        // THE TESTED OPERATION
        haystack.push ( create_UUID () )
    }

    const { performance } = require('perf_hooks');
    let results = []
    const iterations = 10
    for ( let counter = 0; counter < iterations; counter ++ ) {
        const needle = create_UUID()
        const start = performance.now()
        const found = haystack.includes( needle )
        const end = performance.now()
        results.push ( end - start )
    }

    const meanOf = (array) => array.reduce((a, b) => a + b) / array.length;

    callback ( null, `${meanOf ( results )} ms was the average time of ${iterations} iterations.` )
    
};
