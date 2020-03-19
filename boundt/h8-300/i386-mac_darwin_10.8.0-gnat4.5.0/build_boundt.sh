#!/bin/bash
#
# build_boundt -- Build the Bound-T time and stack analyser.
#                 This script for the H8/300 version of Bound-T,
#                 built on i386-mac_darwin_10.8.0-gnat4.5.0.
#
# Copyright (c) 2014 Tidorum Ltd.
#
# $Revision: 1.1 $
# $Log: build_boundt.sh,v $
# Revision 1.1  2014/02/08 22:04:23  niklas
# First version.
#

if [ $# -lt 1 ]; then
   echo "Usage:" `basename $0` "licencedir..." >&2
   exit 1
fi

# The choice of components, platforms, and licences:

source gnat-env-vals $*

gnatopt="-g -O2 -gnato -fstack-check"

# Most of the work:

gnatmake ${gnatopt} boundt_h8_300

# The semi-statically linked version:

gnatmake \
    ${gnatopt} \
    -o boundt_h8_300.static \
    boundt_h8_300 \
    -bargs -static
