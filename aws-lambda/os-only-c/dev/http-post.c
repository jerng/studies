/***************************************************************************
 *                                  _   _ ____  _
 *  Project                     ___| | | |  _ \| |
 *                             / __| | | | |_) | |
 *                            | (__| |_| |  _ <| |___
 *                             \___|\___/|_| \_\_____|
 *
 * Copyright (C) Daniel Stenberg, <daniel@haxx.se>, et al.
 *
 * This software is licensed as described in the file COPYING, which
 * you should have received as part of this distribution. The terms
 * are also available at https://curl.se/docs/copyright.html.
 *
 * You may opt to use, copy, modify, merge, publish, distribute
 * and/or sell
 * copies of the Software, and permit persons to whom the Software
 * is
 * furnished to do so, under the terms of the COPYING file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT
 * WARRANTY OF ANY
 * KIND, either express or implied.
 *
 * SPDX-License-Identifier: curl
 *
 ***************************************************************************/
/* <DESC>
 * simple HTTP POST using the easy interface
 * </DESC>
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

/* inits */
char* _REQUEST_URI;
char* _INVOCATION_ID;
static size_t _HEADER_CALLBACK(
        char *buffer,
        size_t size,
        size_t nitems,
        void *userdata
        ) 
{ 
    printf("\nGET response header : %s",buffer) ;
    return nitems * size ;
};
void* _EVENT_DATA ;
char* _RESPONSE_URI ;
char* _RESPONSE ;

int main(void)
{
    CURL *curl ;
    CURLcode res ;

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
        curl_easy_setopt(curl, CURLOPT_URL, "127.0.0.1");
        curl_easy_setopt(curl, CURLOPT_PORT, 8080L);
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        
        curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, _HEADER_CALLBACK);
        // Alternatively use CURLOPT_HEADERDATA to write to a FILE
        
        printf("\nRESUME WORK HERE : https://curl.se/libcurl/c/getinmemory.html");
        // RESUME WORK HERE : https://curl.se/libcurl/c/getinmemory.html
        //curl_easy_setopt(curl, CURLOPT_WRITEDATA, &_EVENT_DATA);

        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed to GET request from Lambda Runtime: %s\n",
                    curl_easy_strerror(res));

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
        printf("\nResponse URI : %s",_RESPONSE_URI);

        curl_easy_setopt(curl, CURLOPT_URL, "127.0.0.1");
        curl_easy_setopt(curl, CURLOPT_PORT, 8080L);
        /* Now
         * specify
         * the
         * POST
         * data
         * */
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, _EVENT_DATA);
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed to POST response to Lambda Runtime: %s\n",
                    curl_easy_strerror(res));

        /* Close connections */
        curl_easy_cleanup(curl);
    }   
    curl_global_cleanup();
    free(_REQUEST_URI);
    free(_RESPONSE_URI);
    return
        0;
}
