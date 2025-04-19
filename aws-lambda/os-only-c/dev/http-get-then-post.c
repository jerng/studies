/*

MINIMAL AL2023 kernel - may required CLI
https://docs.aws.amazon.com/linux/al2023/ug/ec2.html

AL2023 compilation :
    sudo dnf install gcc libcurl-devel -y
    gcc run.c -o run-dynamic -lcurl -O3 -Os && strip run-dynamic
*/

#define _GNU_SOURCE

#define MAX_GROUPS 2

#include <curl/curl.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* inits */
char* _REQUEST_URI;
regex_t regex;
regmatch_t groups[MAX_GROUPS];
char _INVOCATION_ID[36];
const char *needle = " *Lambda-Runtime-Aws-Request-Id *: *([^[:space:]]*)";  
static size_t _REQUEST_HEADER_CALLBACK(
        char* buffer,
        size_t size, // ? : always 1, see manual
        size_t nitems,
        void* userdata
        )
{
    printf("\nGET response header : %s",buffer) ;
    if (regexec(&regex, buffer, MAX_GROUPS, groups, 0) == 0) 
    {
        strncpy( _INVOCATION_ID, 
                ( buffer + groups[1].rm_so ),
                ( groups[1].rm_eo - groups[1].rm_so )
               );
        printf( "\nMatch found : start : %i, end : %i, '%s'\n", 
                groups[1].rm_so,
                groups[1].rm_eo,
                _INVOCATION_ID
              );
    } else {
        printf("\nNo match found\n");
    }
    return nitems * size ;
};

struct _REQUEST_WRITEDATA_STRUCT 
{
    char* _MEMORY;
    size_t _SIZE;
};

static size_t _REQUEST_WRITEDATA_CALLBACK(
        char* buffer,
        size_t size, // always 1, see manual
        size_t nmemb,
        void* userdata
        )
{
    size_t _TOTAL_INPUT_SIZE = size * nmemb;
    struct _REQUEST_WRITEDATA_STRUCT*__EVENT_DATA_STRUCT 
        = (struct _REQUEST_WRITEDATA_STRUCT*) userdata;

    char* pointer =
        realloc(__EVENT_DATA_STRUCT->_MEMORY,__EVENT_DATA_STRUCT->_SIZE +
                _TOTAL_INPUT_SIZE + 1);
    if(!pointer) {
        /* out of memory */
        fprintf(stderr,
                "\n_REQUEST_WRITEDATA_CALLBACK/4 : "
                "not enough memory : realloc returned NULL\n");
        return 0;
    }
    __EVENT_DATA_STRUCT->_MEMORY = pointer;
    memcpy( &( __EVENT_DATA_STRUCT->_MEMORY[
                __EVENT_DATA_STRUCT->_SIZE
    ]
    ), 
            buffer,
            _TOTAL_INPUT_SIZE 
          );
    __EVENT_DATA_STRUCT->_SIZE += _TOTAL_INPUT_SIZE;
    __EVENT_DATA_STRUCT->_MEMORY[ __EVENT_DATA_STRUCT->_SIZE ] = 0;

    return _TOTAL_INPUT_SIZE;

}

char* _RESPONSE_URI ;
struct curl_slist* _RESPONSE_HEADERS = NULL;

int main(void)
{
    CURL *curl ;
    CURLcode res ;

    // Compile the regular expression
    if (regcomp(&regex, needle, REG_EXTENDED)) {
        printf("\nCould not compile regex");
        return 1;
    }

    /* Gets AWS Lambda ENV */
    //char* _HANDLER = getenv("_HANDLER") ;
    char* _AWS_LAMBDA_RUNTIME_API = getenv("AWS_LAMBDA_RUNTIME_API") ;

    /* In Windows, this inits the Winsock stuff
     * */
    curl_global_init(CURL_GLOBAL_ALL) ;

    /* get a curl handle */
    curl = curl_easy_init() ;
    if(curl) {

        /* GET : next request from Lambda Runtime */

        // discard return values ( should check for success later )
        asprintf( &_REQUEST_URI,
                "http://%s/2018-06-01/runtime/invocation/next",
                _AWS_LAMBDA_RUNTIME_API
                );
        printf("\nRequest URI : %s",_REQUEST_URI);

        //curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, xxx);
        curl_easy_setopt(curl, CURLOPT_URL, _REQUEST_URI);
        //curl_easy_setopt(curl, CURLOPT_URL, "127.0.0.1");
        //curl_easy_setopt(curl, CURLOPT_PORT, 8080L);
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

        curl_easy_setopt(   curl, 
                CURLOPT_HEADERFUNCTION, 
                _REQUEST_HEADER_CALLBACK
                );
        // Alternatively use CURLOPT_HEADERDATA to write to a FILE
        curl_easy_setopt(   curl, 
                CURLOPT_WRITEFUNCTION, 
                _REQUEST_WRITEDATA_CALLBACK
                );

        struct _REQUEST_WRITEDATA_STRUCT _EVENT_DATA_STRUCT;
        _EVENT_DATA_STRUCT._MEMORY = malloc(1);
        _EVENT_DATA_STRUCT._SIZE = 0;
        curl_easy_setopt(   curl, 
                CURLOPT_WRITEDATA, 
                (void *)&_EVENT_DATA_STRUCT
                );

        res = curl_easy_perform(curl);
        if(res != CURLE_OK){
            fprintf(stderr, "curl_easy_perform() failed to GET request from Lambda Runtime: %s\n",
                    curl_easy_strerror(res));
        }else{
            printf("Request EVENT_DATA : %s",_EVENT_DATA_STRUCT._MEMORY);
        }

        /* Reset options only */
        curl_easy_reset(curl);

        /* DO NOT Close connections */
        // curl_easy_cleanup(curl);

        /* POST : response to Lambda Runtime */

        // discard return values ( should check for success later )
        asprintf( &_RESPONSE_URI, 
                "http://%s/2018-06-01/runtime/invocation/%s/response",
                _AWS_LAMBDA_RUNTIME_API,
                _INVOCATION_ID
                );
        printf("\nResponse URI : %s\n",_RESPONSE_URI);

        //curl_easy_setopt(curl, CURLOPT_URL, "127.0.0.1");
        curl_easy_setopt(curl, CURLOPT_URL, _RESPONSE_URI);
        //curl_easy_setopt(curl, CURLOPT_PORT, 8080L);

        _RESPONSE_HEADERS = curl_slist_append(
                _RESPONSE_HEADERS,
                "Content-Type: text/plain");
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, _RESPONSE_HEADERS);
        /* Now
         * specify
         * the
         * POST
         * data
         * */
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, _EVENT_DATA_STRUCT._MEMORY);
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed to POST response to Lambda Runtime: %s\n",
                    curl_easy_strerror(res));

        /* Close connections */
        curl_easy_cleanup(curl);
        curl_slist_free_all(_RESPONSE_HEADERS);
        free(_EVENT_DATA_STRUCT._MEMORY);
    }   
    curl_global_cleanup();
    regfree(&regex);
    free(_REQUEST_URI);
    free(_RESPONSE_URI);
    return 0;
}
