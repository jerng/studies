function handler () {
    EVENT_DATA=$1

    # RESPONSE="{\"statusCode\": 200, \"body\": \"Hello from Lambda!\"}"
    # 
    RESPONSE="The EVENT_DATA : $EVENT_DATA"
    
    echo $RESPONSE
}
