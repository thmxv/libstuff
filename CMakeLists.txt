cmake_minimum_required( VERSION 3.15 )

project( stuff VERSION 1.0 LANGUAGES CXX )

# Stuff library
# As long as the lib stay header only (INTERFACE)
# all the sources/build properties do noit need to be set
# set( LIBSTUFF_PUBLIC_HEADERS 
#     string.h )
# set ( LIBSTUFF_SOURCE_FILES files.cxx )
add_library( libstuff INTERFACE )
#     ${LIBSTUFF_PUBLIC_HEADERS} 
#     ${LIBSTUFF_SOURCE_FILES} )
target_include_directories( libstuff INTERFACE include )
target_compile_features( libstuff INTERFACE cxx_std_20 )
# set_target_properties( libstuff PROPERTIES CXX_EXTENSIONS OFF )

# Tests
enable_testing()
function( libstuff_test name)
    add_executable( test_${name} test/${name}.cxx )
    target_link_libraries( test_${name} PRIVATE libstuff )
    target_compile_options( test_${name} PRIVATE -Werror -W -Wall -Wextra -Wconversion -Wsign-conversion -pedantic -pedantic-errors)
    target_compile_features( test_${name} PRIVATE cxx_std_20 )
    set_target_properties( test_${name} PROPERTIES CXX_EXTENSIONS OFF )
    add_test( ${name} test_${name} )
endfunction()

libstuff_test(string)
