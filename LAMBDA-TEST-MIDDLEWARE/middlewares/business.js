'use strict'

// PROJECT
const mark      = require ( '../modules/mark' )            

// NODE
const fs            = require ( 'fs' )
const querystring   = require ( 'querystring' )

// VIEW
let cssMilligram = fs.readFileSync ( 'io/blobs/milligram.min.css', { encoding: 'utf8' } )
mark ( `milligram.min.css READ` )

let htmlIndex = fs.readFileSync ( 'io/blobs/index.html', { encoding: 'utf8' } )
mark( `index.html READ`)

const tableOfTableSchemas   = require (`../markup/table-of-table-schemas.js`) 
        
// DB

const business = async ( data ) => {

///////////////////////////////////////////////////////////////////////////////

    if ( ! data.LAMBDA.event.requestContext ) {
        return 'test environment : OK'
    }
    /*
    const qParams = data.LAMBDA.event.queryStringParameters
    if ( ! qParams ) 
    {
    //    data.RU.response.redirectURL = data.LAMBDA.event.requestContext.http.path
        
        return data
    }
    else 
    if (  qParams.file )
    {
        switch ( qParams.file )
        {
            case ('milligram.min.css') :
                
                data.RU.response.sendBlob = {
                    'content-type' : 'text/css',
                    body: cssMilligram
                }
                return data

            default:
                data.RU.response.body  
                = `Query parameter "file" was present, but no file was specified.`
                return data
        }
    }
    else 
    {
    
    
    
    
        switch (qParams.ruthenium) 
        {
            case ( 'initial' ) :
         
                break
            case ( 'form' ) : 
                return data
            case ( undefined ) : 
                data.RU.response.redirectURL 
                    = data.LAMBDA.event.requestContext.http.path 
                    + `?ruthenium=initial&server-says=query parameters did not include "ruthenium"; you were redirected`
                return data
        }

///////////////////////////////////////////////////////////////////////////////
        data.RU.response = { 
            statusCode: 200,
            headers: {
                'content-type' : 'text/html'
            },
            body: htmlIndex
            
        }
        return data
        
        
        
        
    }*/
}
module.exports = business
mark (`business.js LOADED`)