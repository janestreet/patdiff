#!/bin/bash

$(dirname $(readlink -f $0))/../../ansicodes/bin/ansicodes.exe minimize |
    $(dirname $(readlink -f $0))/../../ansicodes/bin/ansicodes.exe visualize |
    sed \
        -e 's/│/|/g' \
        -e 's/┌/|/g' \
        -e 's/┐/|/g' \
        -e 's/└/|/g' \
        -e 's/┘/|/g' \
        -e 's/┼/|/g' \
        -e 's/├/|/g' \
        -e 's/┤/|/g' \
        -e 's/─/-/g' \
        -e 's/┬/-/g' \
        -e 's/┴/-/g' \
        -e 's/\[37m/[white]/g' \
        -e 's/\[36m/[cyan]/g' \
        -e 's/\[35m/[magenta]/g' \
        -e 's/\[34m/[blue]/g' \
        -e 's/\[33m/[yellow]/g' \
        -e 's/\[32m/[green]/g' \
        -e 's/\[31m/[red]/g' \
        -e 's/\[30m/[black]/g' \
        -e 's/\[0m/[off]/g' \
        -e 's/\[0;0m/[off]/g' \
        -e 's/\[2m/[dim]/g' \
        -e 's/\[0;1m/[off][bold]/g' \
        -e 's/\[0;31m/[off][red]/g' \
        -e 's/\[0;32m/[off][green]/g' \
        -e 's/\[0;2m/[off][dim]/g' \
        -e 's/\[0;100;30m/[off][high-intensity:bg:black][black]/g' \
        -e 's/\[0;41;30m/[off][bg:red][black]/g' \
        -e 's/\[0;42;30m/[off][bg:green][black]/g' \
        -e 's/\[0;43;30m/[off][bg:yellow][black]/g' \
;
