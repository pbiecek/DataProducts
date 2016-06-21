cmake_minimum_required(VERSION 2.8)
project(jnpcleaner)

option(DEBUG OFF)

if (DEBUG)
    set(CMAKE_BUILD_TYPE DEBUG)
else (DEBUG)
    set(CMAKE_BUILD_TYPE RELEASE)
endif (DEBUG)

set(CMAKE_CXX_FLAGS_DEBUG "-std=c++11 -Wall -pedantic -g")
set(CMAKE_CXX_FLAGS_RELEASE "-std=c++11 -O3")

set(SOURCE_FILES
        src/main.cpp
        src/JnpCleaner.cc
        src/JnpCleaner.hh
        src/GradeCleaner.cc
        src/GradeCleaner.hh
        src/QuestionnaireCleaner.cc
        src/QuestionnaireCleaner.hh)

add_executable(jnpcleaner ${SOURCE_FILES})
