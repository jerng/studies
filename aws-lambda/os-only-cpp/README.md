# Performance check

```
this C++ test : sending entire EVENT_DATA slug : 2025-04-22

Billed Duration:    112     ms      1     ms best case without init
Init Duration:       98.65  ms
Duration:            13.06  ms      0.86  ms best case without init
Memory Size:        128     MB
Max Memory Used:     28     MB     30     MB stabilised 
```

# Modified demo workflow

from [here](https://github.com/awslabs/aws-lambda-cpp/)

```
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
```
