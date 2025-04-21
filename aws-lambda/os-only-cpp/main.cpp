#include <aws/lambda-runtime/runtime.h>
#include <iostream>

using namespace aws::lambda_runtime;

static invocation_response my_handler(invocation_request const& req)
{
    std::cout << "my_handler : enter" << std::endl;
    //std::cout << "stdout : req.payload : " << req.payload << std::endl;


    //    if (req.payload.length() > 42) {
    //    std::cout << "my_handler : exit fail" << req.payload << std::endl;
    //        return invocation_response::failure("error message here"/*error_message*/,
    //                "error type here" /*error_type*/);
    //    }

    std::cout << "my_handler : exit success" << std::endl;
    return invocation_response::success(req.payload /*payload*/,
            "application/json" /*MIME type*/);
}

int main()
{
    std::cout << "main : enter" << std::endl;
    run_handler(my_handler);
    std::cout << "main : exit" << std::endl;
    return 0;
}
