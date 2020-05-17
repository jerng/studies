'use strict'

//  Provide a debuggable function name, 
//  in order to avoid debuggin (function).toString()

const thisIsMyName = async ( data ) => {
    
    return data
}

module.exports = thisIsMyName
const mark      = require ( '../mark' )            
mark ( `thisIsMyName.js LOADED` )