#!/bin/bash

LUAU_SOURCES="dependencies/Luau/Analysis/src/*.cpp dependencies/Luau/Ast/src/*.cpp dependencies/Luau/Config/src/*.cpp dependencies/Luau/VM/src/*.cpp"
LUAU_INCLUDE="-Idependencies/Luau/Analysis/include -Idependencies/Luau/Ast/include -Idependencies/Luau/Common/include -Idependencies/Luau/Config/include -Idependencies/Luau/VM/src -Idependencies/Luau/VM/include"

LUAU_SOURCES_BUILD=$(echo $LUAU_SOURCES | sed 's/dependencies\//..\/..\/..\/dependencies\//g' -)
LUAU_INCLUDE_BUILD=$(echo $LUAU_INCLUDE | sed 's/dependencies\//..\/..\/..\/dependencies\//g' -)

PLATFORM=linux
RELEASE_FLAGS=
STATIC_FLAGS=-static
ASAN_FLAGS=
TEST_DEFINES=

while [[ $# -gt 0 ]]; do
    case $1 in
        linux)
            PLATFORM=linux
            shift
            ;;
        windows)
            PLATFORM=windows
            shift
            ;;
        --release)
            RELEASE_FLAGS=-O2
            shift
            ;;
        --nostatic)
            STATIC_FLAGS=
            shift
            ;;
        --test)
            TEST=true
            shift
            ;;
        --failtests)
            TEST_DEFINES=-DFAILTESTS
            shift
            ;;
        --asan)
            ASAN_FLAGS=-fsanitize=address
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

if [[ $TEST ]]; then
    outname=test-luau-format
else
    outname=luau-format
fi

outfile=build/$outname

if [[ "$PLATFORM" == "linux" ]]; then
    compiler=g++
    builddir=build/linux
elif [[ "$PLATFORM" == "windows" ]]; then
    compiler=x86_64-w64-mingw32-g++-posix
    if ! command -v $compiler >/dev/null 2>&1; then
        compiler="x86_64-w64-mingw32-g++"
    fi
    outfile=$outfile.exe
    builddir=build/windows
else
    echo "unknown platform"
    exit 1
fi

if [ ! -d "build" ]; then
    mkdir build
fi

if [ ! -d "$builddir" ]; then
    mkdir $builddir
fi

luaubuilddir=$builddir/Luau
if [ ! -d "$luaubuilddir" ]; then
    mkdir $luaubuilddir
    cd $luaubuilddir
    echo "building luau..."
    $compiler -std=c++17 -g -O2 -c $LUAU_SOURCES_BUILD $LUAU_INCLUDE_BUILD
    ar rcs libluau.a *.o
    echo "luau built!"
    cd ../../..
fi

luauformatbuilddir=$builddir/luau-format
if [ -d $luauformatbuilddir ]; then
    rm -r $luauformatbuilddir
fi
mkdir $luauformatbuilddir

echo "building luau-format..."
pushd $luauformatbuilddir
$compiler -std=c++17 -g -Wall $RELEASE_FLAGS $ASAN_FLAGS -c ../../../src/* -I../../../include $LUAU_INCLUDE_BUILD -L../Luau -lluau
ar rcs libluau-format.a *.o
popd
echo "luau-format built"

if [[ $TEST ]]; then
    echo "building test-luau-format..."
    if [[ ! $TEST_DEFINES = "" ]]; then
        echo "NOTE: tests will fail due to --failtests being passed"
    fi
    $compiler -std=c++17 $ASAN_FLAGS $STATIC_FLAGS -o $outfile -g -Wall tests/* -Itests -Iinclude $LUAU_INCLUDE $TEST_DEFINES -L$luauformatbuilddir -lluau-format -L$luaubuilddir -lluau
    echo "tests built to $outfile"
    exit
fi

echo "buildling cli..."
$compiler -std=c++17 -g -Wall $RELEASE_FLAGS $ASAN_FLAGS $STATIC_FLAGS -o $outfile main.cpp -Iinclude $LUAU_INCLUDE -L$luauformatbuilddir -lluau-format -L$luaubuilddir -lluau
echo "cli built to $outfile"
