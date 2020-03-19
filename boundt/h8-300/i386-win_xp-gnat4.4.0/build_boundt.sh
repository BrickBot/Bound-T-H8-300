#!/bin/bash
#
# build_boundt -- Build the Bound-T time and stack analyser.
#                 This script for the H8/300 version of Bound-T,
#                 built on i386-win_xp-gnat4.4.0.
#
# Copyright (c) 2011 Tidorum Ltd.
#
# $Revision: 1.1 $
# $Log: build_boundt.sh,v $
# Revision 1.1  2011/05/11 21:02:11  niklas
# First version.
#

if [ $# -lt 1 ]; then
   echo "Usage:" `basename $0` "licencedir..." >&2
   exit 1
fi

# The choice of components, platforms, and licences:

source gnat-env-vals $*

gnatopt="-g -O2 -gnato"
# The option -fstack-check is omitted because it makes
# GNAT bomb out on some source files.

# Most of the work:

gnatmake ${gnatopt} boundt_h8_300

# For MS Windows, there is no separate statically
# linked version; only the normal .exe.
