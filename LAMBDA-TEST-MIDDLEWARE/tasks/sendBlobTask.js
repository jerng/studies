'use strict'

// NODE
const fs    = require ( 'fs' )
const blobs = {}

// SAFELIST - until we add something like https://www.npmjs.com/package/mime
const blobMimeTypes = {
    'index.html':`text.html`,
    'milligram.min.css':`text/css`
}


const blobFileNames = fs.readdirSync ('io/blobs')
blobFileNames.forEach ( ( current, index, array ) => {
    blobs[ current ] = fs.readFileSync ( 'io/blobs/' + current )
} /* , thisArg */ ) 


const sendBlobTask = async ( data ) => {
    
    data.RU.response.sendBlob = {
        'body':         blobs [ data.RU.queryStringParameters.file ],
        'content-type': blobMimeTypes [ data.RU.queryStringParameters.file ]
    } 
        
    
    // no need to return data
}

module.exports  = sendBlobTask
const mark      = require ( '../modules/mark' )            
mark ( `sendBlobTask.js LOADED` )