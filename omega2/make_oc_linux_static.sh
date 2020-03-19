# Making oc on Linux for delivery with Bound-T.
# Static linking is used.
#
# Niklas Holsti, Tidorum Ltd.
# $Id: make_oc_linux_static.sh,v 1.1 2009/12/04 18:11:41 niklas Exp $

make LEXFLAGS=--always-interactive oc \
     TARGET_LDFLAGS=-static
