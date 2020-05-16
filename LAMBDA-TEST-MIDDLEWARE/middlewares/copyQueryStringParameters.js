const copyQueryStringParameters = async ( data ) => {

    data.RU.queryStringParameters = 
        data.LAMBDA.event.queryStringParameters
        ?   data.LAMBDA.event.queryStringParameters
        :   {}
    // Values with same key stored as: CSV string
                        
    return data
}

module.exports = copyQueryStringParameters
const mark = require ('../modules/mark')
mark (`copyQueryStringParameters.js LOADED`)