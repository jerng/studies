'use strict'

// PROJECT
const mark      = require ( 'mark' )            

// NODE
const fs        = require ( 'fs')
const util      = require ( 'util')

// NODE - customised
fs.pReadFile    = util.promisify ( fs.readFile )
    // TODO: Compare with fs.createReadStream

// VIEW
let cssMilligram
fs.pReadFile ( './browsable/milligram.min.css', 'utf8' ).then( fValue => { cssMilligram = fValue } /*, onR */ )
mark ( `milligram.min.css READ` )

let htmlIndex
fs.pReadFile ( './browsable/index.html', 'utf8' ).then( fValue => { htmlIndex = fValue } /*, onR */ )
mark( `index.html READ`)
        
// DB
const AWS = require ( 'aws-sdk' ) 

AWS.config.apiVersions = {
    dynamodb: '2012-08-10'
}
const DDB = new AWS.DynamoDB ()
const DC = new AWS.DynamoDB.DocumentClient () 

const business = async ( data ) => {
    
console.log ( 
    `business.js :`, 
    //data 
)            
    /*    
    
        let result1 = await DDB.listTables ().promise() 
        console.log ( `listTables COMPLETED ( `, Math.round ((tempMark = performance.now()) - lastMark), `, `, Math.round(lastMark = tempMark), `)` )
    
        let result2 = await DDB.describeTable ( { TableName : 'TEST-SESSIONS' } ).promise()
        console.log ( `describeTable COMPLETED ( `, Math.round ((tempMark = performance.now()) - lastMark), `, `, Math.round(lastMark = tempMark), `)` )
        
        let result3 = await DC.put ( {
            TableName: 'TEST-SESSIONS',
            Item: {
                session_id : 'I ought to be a uuid',
                something: 123,
                somethingelse : 12345
            },
            ReturnConsumedCapacity : 'TOTAL'
        } ).promise()
        console.log ( `put COMPLETED ( `, Math.round ((tempMark = performance.now()) - lastMark), `, `, Math.round(lastMark = tempMark), `)` )
    
        let result4 = await DC.get ( {
            TableName: 'TEST-SESSIONS',
            Key: {
                'session_id' : 'I ought to be a uuid'
            },
            ReturnConsumedCapacity : 'TOTAL'
        } ).promise()
        console.log ( `get COMPLETED ( `, Math.round ((tempMark = performance.now()) - lastMark), `, `, Math.round(lastMark = tempMark), `)` )
    
        let result5 = await DC.delete ( {
            TableName: 'TEST-SESSIONS',
            Key: {
                'session_id' : 'I ought to be a uuid'
            },
            ReturnConsumedCapacity : 'TOTAL'
        } ).promise()
        console.log ( `delete COMPLETED ( `, Math.round ((tempMark = performance.now()) - lastMark), `, `, Math.round(lastMark = tempMark), `)` )
    */
    ///////////////////////////////////////////////////////////////////////////////
    /*
        const operation = await DC.put ( {
            TableName: 'TEST-APP-GRID-SCHEMAS',
            Item: {
                'grid' : 'shoes',
                'columns' : [
                    'color',
                    'size',
                    'material'
                ]
            },
            ReturnConsumedCapacity : 'TOTAL'
        } ).promise()
        const operation // = `` // read file and spit to view
            = await DDB.describeTable ( { TableName : 'TEST-APP-GRID-SCHEMAS' } ).promise()
    
        console.log ( `operation COMPLETED ( `, Math.round ((tempMark = performance.now()) - lastMark), `, `, Math.round(lastMark = tempMark), `)` )
        
        return JSON.stringify( {    context: context,
                    operation : operation
        
                    listTables: result1,
                    describeTable: result2,
                    put: result3,
                    get: result4,
                    delete: result5,
    
        }, null, 4 ) 
    */
    ///////////////////////////////////////////////////////////////////////////////
    
    
    
    ///////////////////////////////////////////////////////////////////////////////

//console.log(`business 109:`,data)

            if ( ! data.LAMBDA.event.requestContext ) {
                return 'test environment : OK'
            }
            const qParams = data.LAMBDA.event.queryStringParameters
            if ( ! qParams ) 
            {
                return {
                    statusCode: 303,
                    headers: {
                        'location' : data.LAMBDA.event.requestContext.http.path 
                                    + `?state=initial&server-says= no query parameters found; you were redirected`
                    }
                }
            }
            else 
            if (  qParams.file )
            {
                switch ( qParams.file )
                {
                    case ('milligram.min.css') :
                        
                        return { 
                            statusCode: 200,
                            headers: { 'content-type' : 'text/css' },
                            body: cssMilligram
                        }
        
                    default:
                        return `Query parameter "file" was present, but no file was specified.`
                }
            }
            else 
            {
                switch (qParams.state) 
                {
                    case ( 'initial' ) :
                        
                        let schemas = await DC.scan ( {
                            TableName: 'TEST-APP-GRID-SCHEMAS',
                            ReturnConsumedCapacity : 'TOTAL'
                        } ).promise()
                        
                        mark( `scan COMPLETED 1` )
    ///////////////////////////////////////////////////////////////////////////////
           
    let markupSchemas = schemas.Items.reduce ( ( accumulator, current, index ) => {
        return accumulator + `
            <tr>
                <td>
    <!----------------------------------------------------------------------------->
    <!-- contentEditable
                onclick="
                this.contentEditable = true;
                const text = this.firstChild,
                    r = document.createRange(),
                    s = window.getSelection();
                r.setStart(text,0);
                r.setEnd(text,text.length);
                s.removeAllRanges();
                s.addRange(r);
    -->        
    
    <blockquote>
    <form method="post" action="/test-middleware?state=form">
    <fieldset onclick=" 
        Array.prototype.forEach.call( 
            this.querySelectorAll('.toggle-set-1'), 
            e => {
                const wasVisible = ['','initial'].includes(e.style.display);
                e.style.display = wasVisible ? 'none' : 'initial';
                if ( (! wasVisible) ) { 
                    const input = this.querySelector('#unlock-table-rename-NAME')
                    input.focus();
                    input.select();
                };
            } 
        );
    ">
        <label for="unlock-table-rename-NAME">
            <h1> ${ current.grid } </h1>
            <button title="click to rename this table" class="button-outline" onclick="return false;"> 
                <span class="toggle-set-1">
                    <i class="material-icons">lock</i>rename</span>
                <span class="toggle-set-1" style="display:none;">
                    lock code: 234806</span>
            </button>
        </label>
        <input  type="text" 
                id="unlock-table-rename-NAME" 
                placeholder="type the code, to unlock this form" 
                class="toggle-set-1"
                style="display:none;"
                onclick="(e=>e.stopImmediatePropagation())(event)"
                oninput="if (this.value==234806) { 
                    alert ('dev: reset required ')
                    const confirmed = window.confirm('WARNING : renaming the SHOES table will perform an expensive database operation - select CANCEL to reconsider.')
                    if ( confirmed ) 
                    {
                        const submission = prompt ('Submit a new name for SHOES:')
                        if ( submission ) {
                            alert ('dev: finally POST here ')
                        } else {
                            alert ('dev: cleanup required ')
                        }
                    } else {
    
                        alert ('dev: cleanup required ')
                    }
                }"
                >
        <input type="hidden" name="table-rename-shoes" value="newName">       
        <input type="submit" value="Send PATCH">
    </fieldset>
    </form>
    </blockquote>
    <!--
    <a class="button" href="https://a.scriptless.webpage" title="rename this table" onclick="return false;">Rename</a> 
    -->
    
    <!----------------------------------------------------------------------------->
                </td>
                <td>
                    <h2>
                    <ul class="float-left">
                    ${ current.columns.reduce ( ( acc, curr, ind ) => {
    ///////////////////////////////////////////////////////////////////////////////
    return acc + `
        <li>
            ${ curr }<br>
            <button title="click to rename this column" class="button-outline " onclick="return false;"> 
                <span class="toggle-set-1">
                    <i class="material-icons">lock</i>rename</span>
            </button>
            <button class="button-outline" onclick="return false;">
                <i class="material-icons">lock</i>destroy</button>
        </li>`
    ///////////////////////////////////////////////////////////////////////////////
                    }, '' ) }
                    </ul>
                    </h2>
                    <a class="button float-right" href="#">Create Column</a>
                </td>
            </tr>`
    }, '' )
    
    htmlIndex += `
    <table>
        <thead>
            <tr>
                <th colspan="2">
                    <h6>
                        System is currently aware of ${ schemas.Count } Table Schemas
                        <a class="button float-right" href="#">Create Table</a>
                    </h6>
    
                </th>
            </tr>
            <tr>
                <th><h3>Table Name</h3></th>
                <th><h3>Column Names</h3></th>
            </tr>
        </thead>
        <tbody style="vertical-align:top;">
            ${ markupSchemas }
        </tbody>
        <tfoot>
            <tr>
                <td colspan="2">
                    <a class="button float-right" href="#">Create Table</a>
                </td>
            </tr>
        </tfoot>
    </table>`
    
    htmlIndex += `<pre><code>${ JSON.stringify(schemas,null,4)}</code></pre>`
    
    
    ///////////////////////////////////////////////////////////////////////////////
                    
                    break
                case ( 'form' ) : 
                    if (    data.LAMBDA.event.headers['content-type'] 
                                == 'application/x-www-form-urlencoded'
                            &&
                            data.LAMBDA.event.isBase64Encoded
                    )
                    {
                        const decoded 
                            = Buffer.from ( data.LAMBDA.event.body, 'base64' )
                                .toString ('utf8')
                    
                        data.RU.form = decoded
                        
                    return  data
                    }
                    switch ( data.LAMBDA.event.requestContext.http.method ) {
                        case ( 'POST' ) :
    ///////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////
                            break
                        case ( 'PUT' ) : 
                        case ( 'POST' ) : 
                        case ( 'DELETE' ) :
                        default:
                    }
                    break
                    
                case ( undefined ) : return {
                        statusCode: 303,
                        headers: {
                            'location' : data.LAMBDA.event.requestContext.http.path 
                                + `?state=initial&server-says=query parameters did not include "state"; you were redirected`
                        }
                    }
            }

            return { 
                statusCode: 200,
                headers: {
                    'content-type' : 'text/html'
                },
                body: htmlIndex + `<code><pre>${JSON.stringify( {
                    data: data
                }, null, 4 ).replace(/\\n/g, '\n') }</pre></code>`
                
            }
        }
    ///////////////////////////////////////////////////////////////////////////////
    
    
    }
module.exports = business
mark (`business.js LOADED`, true)