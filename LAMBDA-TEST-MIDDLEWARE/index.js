'use strict'

// require() executes modules; use require.res() to resolve without execution.

// PROJECT
const mark      = require ( './modules/mark' )            
mark( `index.js required mark.js`)

const ruthenium = require ( './modules/ruthenium' )
const wastems   = async ms => { 
    const start = new Date().getTime() 
    while (new Date().getTime() < start + ms);
}
// JSON.stringify( DATA, null, 4 ).replace(/\\n/g, '\n')
mark( `index.js did other things`)

// PROJECT - MIDDLEWARES, lexical order
const business                  = require (`./middlewares/business.js`) 
const composeResponse           = require (`./middlewares/composeResponse.js`) 
const copyQueryStringParameters = require (`./middlewares/copyQueryStringParameters.js`) 
const getFormData               = require (`./middlewares/getFormData.js`) 
const getHeaders                = require (`./middlewares/getHeaders.js`) 
const lastGuard                 = require (`./middlewares/lastGuard.js`) 
const router                    = require (`./middlewares/router.js`) 

// LAMBDA HANDLER
exports.handler = async function () { 

console.log (
[ `CONTINUE work at ruthenium.js, line 70, implement middlewares for composeResponse, logger, router` ]
)
    
    mark( `index.js, first mark in handler`, true )
    
    return  ruthenium   ( arguments, [  // MIDDLEWARES, execution order
                                
        getHeaders,                 // Values with same key stored as: Array of values
        copyQueryStringParameters,  // Values with same key stored as: CSV string
        getFormData,
        
        router,
        business,
        
        composeResponse,
        lastGuard
    ] ) 
    
}
mark (`index.js LOADED`, true)

