'use strict'

const fs = require ( 'fs' )
const tasks = {}

const taskFileNames = fs.readdirSync ('tasks')
taskFileNames.forEach ( ( current, index, array ) => {
    tasks[ current.slice (0, -3) ] = require ( '../../tasks/' + current )
} /* , thisArg */ ) 


const router = async ( data ) => {
 
    /*  By this point in the pipeline, (data.RU) should provide:    
    *       (.headers)
    *       (.headers.cookies)
    *       (.queryStringParameters)
    *       (.formStringParameters)
    *
    *   And, (data.LAMBDA) also provides:
    *       (.event)
    *       (.context)
    *
    *   So, any of these may be used by (DetermineTaskName)
    */
    
    // DIY here
    const customDetermineTaskName = undefined
    
    // Default here
    const defaultDetermineTaskName = () => {
        
        switch ( data.RU.queryStringParameters.ruthenium )
        {
            case ( 'file' ):
                data.RU.taskName = 'sendBlobTask'
                break
            case ( 'initial' ):
                data.RU.taskName = 'initialTask'
                break
            case ( undefined ):
            default:
                data.RU.response.redirectRoute = 'initial'
        }
    }
    
    // Determine the task.
    customDetermineTaskName 
        ? customDetermineTaskName () 
        : defaultDetermineTaskName ()

////////////////////////////////////////////////////////////////////////////////

    // redirects: short-circuit
    if ( data.RU.response.redirectRoute || data.RU.response.redirectURL ) {
        return data
    }    

    // no redirects: 
    // Run the task, if its module is found.
    //
    //      TODO: perhaps we want another reducer here for multiple tasks?
    //
    if ( data.RU.taskName in tasks  ) {
        await tasks [ data.RU.taskName ]( data )   
    }
    else {
        throw Error ( `Could not find (${ data.RU.taskName 
                        }) in the tasks directory.` )
    }

////////////////////////////////////////////////////////////////////////////////
    
    return data
}

module.exports = router
const mark      = require ( '../mark' )            
mark ( `router.js LOADED` )