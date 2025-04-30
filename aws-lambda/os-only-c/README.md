1. For final results, see *bootstrap` tries to reuse resources, has a do-while loop*
2. Where the files in this folder, come from ...

## `dev` : prototyping in the C language

#### `bootstrap` tears down after each request, does not reuse resources
- `http-get-then-post.c` attempts to replicate the functionality of
  `samples-modified`
- it does not have to be *statically* compiled
- whereas, binaries uploaded via web console to a Lambda Layer, are executable
  at the Lambda runtime's `/opt/` folder; and so are binaries uploaded via web
  console to the Lambda Code Source tab as a `.zip`
    - however, binaries uploaded via web console to the Lambda runtime's
      `/var/task/` folder do not seem to be executable, even if they are set
      with executable permissions ... this is not clearly documented, but it may
      be a security precaution
- so `/var/task/bootstrap`, a binary executable is uploaded as a `.zip` to
  Lambda Code Source tab in the web console
- `bootstrap`'s compilation from `http-get-then-post.c` was successful on a EC2
  `t4g.nano` which uses the same `graviton2` CPU as Lambda's arm64 runtime ...
  the executable binary was then `scp`-ed out before being uploaded to Lambda as
  a layer
    - cross-compilation was NOT successful on Ubuntu 22.04, x86_64, via
      `aarch64-linux-gnu-gcc`, because I am not smart enough to finish this work
 
## Test 4

>   custom C runtime, `bootstrap` tries to reuse resources, has a do-while loop

- adding a `do{}while()` loop improves performance significantly
```
this C test : sending entire EVENT_DATA slug : 

Billed Duration:     17     ms      1     ms best case without init
Init Duration:       14.81  ms      
Duration:             1.67  ms      0.78  ms best case without init
Memory Size:        128     MB      
Max Memory Used:     21     MB     23     MB best case without init ... stable after 2 seconds at 50 requests/s 
-
Memory Profile stages :

One-time Initialisation :
1.1.    When `bootstrap`'s `main` starts    :  86 MB
1.2.    After init of `curl` `regex`        :  89 MB

First time the Lambda served :
2.1.    Top of the server loop              :  89 MB
2.2.    After GET of request from runtime   :  89 MB
2.3.    After POST of response to runtime   :  90 MB
2.4.    Bottom of the server loop           :  90 MB : but billed 21 MB only : so 69 MB hidden by runtime 

After 1500 requests :
2.4.    Bottom of the server loop           : 103 MB : but billed 23 MB only : so 80 MB hidden by runtime 
```
... averaging 1ms per request, that's USD 0.29 for 10 million requests ...

#### Test 4 : compared with "native Rust runtime"

>   AWS's own [native Rust runtime's Hello
  world](https://github.com/awslabs/aws-lambda-rust-runtime) which clocks in at
  around 1.7ms
```
Rust using Tokio as its async runtime : sending one line hello world :

Billed Duration:     32     ms      2     ms best case without init
Init Duration:       29.96  ms      
Duration:             1.51  ms      1.10  ms best case without init
Memory Size:        128     MB      
Max Memory Used:     16     MB     17     MB best case without init 
```

## Test 3

>   now in C, but without reuse of `curl` and `regex` resources

-  moving from `curl` commands called in `bootstrap.sh` to `libcurl` calls from
   `bootstrap` seemed to reduce resource consumption significantly
```
this C test : sending entire EVENT_DATA slug : 

Billed Duration:     37     ms      5     ms best case without init
Init Duration:       32.15  ms      
Duration:             4.73  ms      4.39  ms best case without init
Memory Size:        128     MB      
Max Memory Used:     24     MB      4     MB best case without init 
```

## Test 2

>   `samples-modified` : still, just Bash command language, and `curl`

- From the AWS web Console, upon creating a Lambda with the "OS Only" Runtime,
  sample files are given by AWS
  - Related documentation :
    https://docs.aws.amazon.com/lambda/latest/dg/runtimes-provided.html
- The sample files are visible in the web console code editor, copied to the
  same directory, renamed appropriately, and edited
- Changes to the sample code :
  - reduced verbosity of `curl`
  - changed `Content-type` to `text/plain`
  - response body is `EVENT_DATA` from Lambda
- CloudWatch performance reported : 

```
Billed Duration:    254     ms 
Init Duration:       27.45  ms 
Duration:           226.50  ms 
Memory Size:        128     MB 
Max Memory Used:     26     MB 
```

## Test 1

>   `samples` : AWS Lambda OS-only Custom Runtime :

A runtime is a program that runs a Lambda function's handler method when the
function is invoked. The runtime can be included in your function's deployment
package, or in a
[layer](https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html).
[Learn more](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html)
about custom Lambda runtimes.

For reference, this function includes a sample
[Bash](https://www.gnu.org/software/bash/) runtime in `bootstrap.sample` and a
corresponding handler file `hello.sh.sample`. As a next step, you should provide
your own bootstrap by either adding a layer implementing a custom runtime or
including a `bootstrap` file in this function's deployment package.
