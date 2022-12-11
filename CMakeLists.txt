cmake_minimum_required(VERSION 3.24)
project(fortran_elements Fortran)

enable_language(Fortran)
set(CMAKE_VERBOSE_MAKEFILE on)

file(GLOB_RECURSE module_sources "src/*.f90")
add_library(modules OBJECT ${module_sources})

add_executable(test_list test/test_list.f90 $<TARGET_OBJECTS:modules>)
add_executable(test_error test/test_error.f90 $<TARGET_OBJECTS:modules>)
add_executable(test_testobj test/test_testobj.f90 $<TARGET_OBJECTS:modules>)
add_executable(test_shared_ptr test/test_shared_ptr.f90 $<TARGET_OBJECTS:modules>)
add_executable(test_dict test/test_dict.f90 $<TARGET_OBJECTS:modules>)

add_library(fortranelements STATIC $<TARGET_OBJECTS:modules>)