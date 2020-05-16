const querystring   = require ( 'querystring' )

const getFormData = async ( data ) => {

    if (    data.LAMBDA.event.headers['content-type'] 
                == 'application/x-www-form-urlencoded'
    )
    {
        data.RU.rawFormString =
            data.LAMBDA.event.isBase64Encoded
            ?   Buffer
                    .from ( data.LAMBDA.event.body, 'base64' )
                    .toString ('utf8')
            :   data.LAMBDA.event.body
            
        data.RU.formStringParameters = 
            querystring.parse ( data.RU.rawFormString )
        
        return  data
    }

    
    return data
}

module.exports  = getFormData
const mark      = require ( '../modules/mark' )            
mark ( `getFormData.js LOADED` )