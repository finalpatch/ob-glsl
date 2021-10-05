project(obglsl)
cmake_minimum_required(VERSION 3.11)

set(CMAKE_CXX_STANDARD 17)

find_package(SDL2 REQUIRED)
find_package(glm REQUIRED)
find_library(SDL2_IMAGE_LIB SDL2_image)
find_library(GLBINDING_LIB glbinding)

file(GLOB SRCS "*.cpp")
add_library(${PROJECT_NAME} SHARED ${SRCS})
target_link_libraries(${PROJECT_NAME} glm SDL2::SDL2-static ${GLBINDING_LIB} ${SDL2_IMAGE_LIB})
