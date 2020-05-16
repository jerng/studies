'use strict'

const mark      = require ( '../modules/mark' )            

// DB
const AWS               = require ( 'aws-sdk' ) 
AWS.config.apiVersions  = { dynamodb: '2012-08-10' }
const DDB               = new AWS.DynamoDB ()
const DC                = new AWS.DynamoDB.DocumentClient () 

const initialTask = async ( data ) => {
    
    data.RU.response.redirectURL = data.LAMBDA.event.requestContext.http.path
        + '?ruthenium=initial'

    data.RU.io.gridSchemasScan = await DC.scan ( {
        TableName: 'TEST-APP-GRID-SCHEMAS',
        ReturnConsumedCapacity : 'TOTAL'
    } ).promise()
    
    mark( `scan COMPLETED` )

    return data
    
}
module.exports = initialTask
mark ( `initialTask.js LOADED` )