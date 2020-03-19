# Making oc on MinGW for use with Bound-T.
#
# Niklas Holsti, Tidorum Ltd.
# $Id: make_oc_mingw.sh,v 1.2 2009/11/14 18:21:59 niklas Exp $

make YYFLAGS=-DYY_ALWAYS_INTERACTIVE \
     LEXFLAGS=-I oc \
     TARGET_LDFLAGS=-static
