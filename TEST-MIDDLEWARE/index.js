'use strict'

// require() executes modules; use require.res() to resolve without execution.

// PROJECT
const mark      = require ( 'mark' )            
mark( `index.js required mark.js`)

const ruthenium = require ( 'ruthenium' )
const wastems   = async ms => { 
    const start = new Date().getTime() 
    while (new Date().getTime() < start + ms);
}
// JSON.stringify( DATA, null, 4 ).replace(/\\n/g, '\n')
mark( `index.js did other things`)

// PROJECT - MIDDLEWARES
const aMiddleware               = require (`./middlewares/aMiddleware.js`) 
const business                  = require (`./middlewares/business.js`) 
const composeResponse           = require (`./middlewares/composeResponse.js`) 
const copyQueryStringParameters = require (`./middlewares/copyQueryStringParameters.js`) 
const getHeaders                = require (`./middlewares/getHeaders.js`) 
const security                  = require (`./middlewares/security.js`) 

// LAMBDA HANDLER
exports.handler = async function () { 
    
    mark( `index.js, first mark in handler`, true )
    
    return  ruthenium   ( arguments, [
                                
        getHeaders,       // Values with same key stored as: Array of values
        copyQueryStringParameters,  // Values with same key stored as: CSV string
        business,
        aMiddleware,
        composeResponse,
        security
    ] ) 
    
}
mark (`index.js LOADED`, true)

console.log (
[ `CONTINUE work at ruthenium.js, line 70, implement middlewares for composeResponse, logger, router` ]
)
