/*

sudo dnf install g++ cmake3 libcurl-devel git -y

git clone https://github.com/awslabs/aws-lambda-cpp.git
cd aws-lambda-cpp
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=~/lambda-install
make && make install

mkdir app
cd app
vi CMakeLists.txt

    cmake_minimum_required(VERSION 3.9)
    set(CMAKE_CXX_STANDARD 11)
    project(demo LANGUAGES CXX)
    find_package(aws-lambda-runtime)
    add_executable(${PROJECT_NAME} "main.cpp")
    target_link_libraries(${PROJECT_NAME} PRIVATE AWS::aws-lambda-runtime)
    target_compile_features(${PROJECT_NAME} PRIVATE "cxx_std_11")
    target_compile_options(${PROJECT_NAME} PRIVATE "-Wall" "-Wextra")

    # this line creates a target that packages your binary and zips it up
    aws_lambda_package_target(${PROJECT_NAME})

vi main.cpp

mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=../../lambda-install
make
make aws-lambda-package-demo

*/
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
