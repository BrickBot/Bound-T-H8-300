#!/bin/bash
#
# build_boundt -- Build the Bound-T time and stack analyser.
#                 This script for the H8/300 version of Bound-T,
#                 built on i386-debian_lenny-gnat4.3.2.
#
# Copyright (c) 2010 Tidorum Ltd.
#
# $Revision: 1.1 $
# $Log: build_boundt.sh,v $
# Revision 1.1  2010/11/30 18:07:43  niklas
# First version.
#
# Revision 1.1  2010-11-29 09:21:12  niklas

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
