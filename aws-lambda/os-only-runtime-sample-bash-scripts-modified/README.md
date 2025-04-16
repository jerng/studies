# Where the files in this folder come from

- From the AWS web Console, upon creating a Lambda with the "OS Only" Runtime, sample files are given by AWS
  - Related documentation : https://docs.aws.amazon.com/lambda/latest/dg/runtimes-provided.html
- The sample files are visible in the web console code editor, copied to the same directory, renamed appropriately, and edited
- Changes to the sample code :
  - reduced verbosity of `curl`
  - changed `Content-type` to `text/plain`
  - response body is `EVENT_DATA` from Lambda
- CloudWatch performance reported : 

```
Billed Duration:    254     ms
    Init Duration:  27.45   ms
    Duration:       226.50  ms
Memory Size:        128     MB
Max Memory Used:    26      MB
```

# Custom AWS Lambda runtime

A runtime is a program that runs a Lambda function's handler method when the function is invoked. The runtime can be included in your function's deployment package, or in a [layer](https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html). [Learn more](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html) about custom Lambda runtimes.

For reference, this function includes a sample [Bash](https://www.gnu.org/software/bash/) runtime in `bootstrap.sample` and a corresponding handler file `hello.sh.sample`. As a next step, you should provide your own bootstrap by either adding a layer implementing a custom runtime or including a `bootstrap` file in this function's deployment package.
