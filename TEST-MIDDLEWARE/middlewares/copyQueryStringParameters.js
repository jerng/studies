module.exports = async ( data ) => {

console.log (
    `copyQueryStringPara:`, 
//    data
)
    
    data.RU.queryStringParameters = data.LAMBDA.event.queryStringParameters
    // Values with same key stored as: CSV string
                        
    return data
}