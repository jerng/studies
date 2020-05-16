// NODE
const fs                    = require ( 'fs' )

// MARKUP
const htmlIndex             = fs.readFileSync ( 'io/blobs/index.html', { encoding: 'utf8' } )
mark( `index.html READ`)
const tableOfTableSchemas   = require (`../markup/table-of-table-schemas.js`) 

const initialTaskMarkup = async ( data ) => {

    htmlIndex 
    
    +=  tableOfTableSchemas ( data.RU.io.gridSchemasScan )
    
    +   `<pre><code>${ JSON.stringify( data.RU.io.gridSchemasScan, null, 4 ) }</code></pre>`
    
    return data
}

module.exports  = initialTaskMarkup
const mark      = require ( '../modules/mark' )            
mark ( `initialTaskMarkup.js LOADED` )