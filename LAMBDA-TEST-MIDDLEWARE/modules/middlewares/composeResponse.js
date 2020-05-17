'use strict'
 
const fs = require ( 'fs' )
const markups = {}

const markupFileNames = fs.readdirSync ('markup')
markupFileNames.forEach ( ( current, index, array ) => {
    markups[ current.slice (0, -3) ] = require ( '../../markup/' + current )
} /* , thisArg */ ) 

const composeResponse = async ( data ) => {
    
    if ( data.RU.response ) {
        
        
        
        
        if ( data.RU.response.redirectRoute ) {
            
            data.RU.response.redirectURL 
                = data.LAMBDA.event.requestContext.http.path
                + '?ruthenium='
                + data.RU.response.redirectRoute
            
            // cleanup
            delete data.RU.response.redirectRoute
        }
        
        
        
        
        if ( data.RU.response.redirectURL ) { 
            
            data.RU.response.statusCode =   data.RU.response.statusCode
                                            ? data.RU.response.statusCode
                                            : 303 // See Other
            
            // ensure headers is an object            
            data.RU.response.headers    =   data.RU.response.headers
                                            ? data.RU.response.headers
                                            : {}
            data.RU.response.headers.location =  data.RU.response.redirectURL 
            
            // cleanup            
            delete data.RU.response.redirectURL
        }
        else
        if ( data.RU.response.sendBlob ) {
            
            data.RU.response.statusCode =   data.RU.response.statusCode
                                            ? data.RU.response.statusCode
                                            : 200 // OK

            // ensure headers is an object            
            data.RU.response.headers    =   data.RU.response.headers
                                            ? data.RU.response.headers
                                            : {}

            // if sendBlob specified a MIME type, then over/write response            
            if ( data.RU.response.sendBlob[ 'content-type' ] ) {
                data.RU.response.headers[ 'content-type' ]
                = data.RU.response.sendBlob[ 'content-type' ]
            } 

            // if response has a MIME type, sent 'nosniff' directive            
            if ( data.RU.response.headers[ 'content-type' ] ) {
                data.RU.response.headers[ 'x-content-type-options' ] = 'nosniff'
            }

            data.RU.response.body = data.RU.response.sendBlob.body

            // cleanup            
            delete data.RU.response.sendBlob
        }
        else
        if ( data.RU.response.markupName ) {
        
            if ( data.RU.response.markupName in markups ) {
                
                // clobber (refine this as above; WIP / TODO )
                data.RU.response = {
                    statusCode: 200,
                    headers: {
                        'content-type': 'text/html'
                    },
                    body: await markups [ data.RU.response.markupName ]( data )
                }
            }
            else {
                throw   Error (`Could not find (${ data.RU.response.markupName 
                        }) in the markups directory. That name was specified at
                        (data.RU.response.markup).`)
            }
        }
        else
        if ( data.RU.taskName ) {
            
            data.RU.inferredMarkupName = data.RU.taskName + 'Markup'
            
            if ( data.RU.inferredMarkupName in markups ) {
                
                // clobber (refine this as above; WIP / TODO )
                data.RU.response = {
                    statusCode: 200,
                    headers: {
                        'content-type': 'text/html'
                    },
                    body: await markups [ data.RU.inferredMarkupName ]( data )
                }
            }
            else {
                throw   Error (`Could not find (${ data.RU.inferredMarkupName }) 
                        in the markups directory. That name was guessed because 
                        (${ data.RU.taskName }) was specified at 
                        (data.RU.taskName).`)
            }
        }
        
        
        
        
    }
    else {
        throw   Error (`(data.RU.response) is falsy. Not sure how to proceed.
                This is usually not a problem as it should be initiated at 
                (ruthenium.js).`)
    }
    
    return data
}

module.exports  = composeResponse
const mark      = require ( '../mark' )            
mark ( `composeResponse.js LOADED` )