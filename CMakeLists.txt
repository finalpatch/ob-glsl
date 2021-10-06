project(ob-glsl-module)
cmake_minimum_required(VERSION 3.11)

set(CMAKE_CXX_STANDARD 17)

find_package(SDL2 REQUIRED)
find_library(SDL2_IMAGE_LIB SDL2_image)
find_library(GLBINDING_LIB glbinding)

file(GLOB SRCS "*.cpp")
add_library(${PROJECT_NAME} SHARED ${SRCS})
set_target_properties(${PROJECT_NAME} PROPERTIES PREFIX "")
target_include_directories(${PROJECT_NAME} PUBLIC "/usr/local/include")
target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARIES} ${SDL2_IMAGE_LIB} ${GLBINDING_LIB})
