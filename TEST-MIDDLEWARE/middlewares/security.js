module.exports = async ( data ) => {
    
    console.log( `security.js` )

    let response // this should already be in (data) already
        = {
            statusCode : 202,
            headers: {
                'content-type' : 'text/html'
            },
            body: `<h1>Status: 202 Accepted</h1>
                ... for the time being, please note : <pre><code>${
                JSON.stringify( data, null, 4 ).replace(/\\n/g, '\n')
            }</code></pre>` 
        }
        
    if ( data.RU.errors.length ) {
        response = {
            statusCode  : 569,
            headers: {
                'content-type' : 'text/html'
            },
            body:   `<h1>Status: 5XX (server is preoccupied)</h1>
                ... peep : <pre><code>${
                JSON.stringify( data.RU.errors, null, 4 ).replace(/\\n/g, '\n')
            }</code></pre>` 
        }
    }
    
    return response 
}