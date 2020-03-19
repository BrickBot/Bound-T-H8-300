# Making oc on Mac OS-X for use with Bound-T.
#
# Niklas Holsti, Tidorum Ltd.
# $Id: make_oc_mac-osx.sh,v 1.2 2009/02/15 10:21:59 niklas Exp $

isada=`which g++ | grep '/ada'`

if [ "${isada}" \!= "" ]; then
   echo "The g++ compiler is found in an 'ada' folder: ${isada}" >&2
   echo "Remove this from PATH to avoid depending on 'ada' libraries." >&2
   exit 1
fi

make LEXFLAGS=--always-interactive oc
