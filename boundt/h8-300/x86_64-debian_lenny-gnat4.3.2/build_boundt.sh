#!/bin/bash
#
# build_boundt -- Build the Bound-T time and stack analyser.
#                 This script for the H8/300 version of Bound-T
#                 under x86_64-debian_lenny-gnat4.3.2.
#
# Copyright (c) 2010 Tidorum Ltd.
#
# $Revision: 1.1 $
# $Log: build_boundt.sh,v $
# Revision 1.1  2010/11/30 19:05:17  niklas
# First version.
#


if [ $# -lt 1 ]; then
   echo "Usage:" `basename $0` "licencedir..." >&2
   exit 1
fi

# The choice of components, platforms, and licences:

source gnat-env-vals $*

gnatopt="-g -O2 -gnato -fstack-check"

# Work-around for GNAT 4.3.2 optimization bug in 64-bit systems:

gnatmake ${gnatopt} -u -s -O0 formats-ubrof.adb
# The -s option triggers recompilation if the existing .o
# was compiled with different options.

# Most of the work:

gnatmake ${gnatopt} boundt_h8_300

# The semi-statically linked version:

gnatmake \
    ${gnatopt} \
    -o boundt_h8_300.static \
    boundt_h8_300 \
    -bargs -static
