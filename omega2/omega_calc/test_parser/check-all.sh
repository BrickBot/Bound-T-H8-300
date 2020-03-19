#!/bin/sh
#
# check-all: Run all the Omega Calculator tests and check output
#
# Usage:
#  - Set the name of the "oc" executable in the PARSER environment var.
#    If the variable is not defined, the script uses "../obj/oc".
#  - Run this script in the "test_parser" directory.
#  - The script runs each test case and uses "diff" to compare the
#    old output (<case>.oc-rt) with the new, ignoring the first
#    output line (the greeting) in both files.
#  - Each failed test is identified on the standard-error output
#    with a "FAILED: <case>" line.
#
# Written by N. Holsti, Tidorum Ltd.
# $Id: check-all.sh,v 1.1 2009/02/27 12:38:09 niklas Exp $

# Use the Omega Calculator binary named by the PARSER variable
# if defined, otherwise the one in ../obj:

calc="${PARSER:-../obj/oc}"

# Temporary files:

ref_out=/tmp/check-all.$$.ref
new_out=/tmp/check-all.$$.new

# Process all files named *.oc-rt; these are the output
# files from the test cases:

my_exit=0

for outfile in *.oc-rt; do

   # The name of the input file is the same as the name
   # of the output file, less the suffix ".oc-rt":

   infile="${outfile%%.oc-rt}"

   echo "Checking ${infile}"

   # Leave out the first (greeting) line from the comparison:

   sed -e '1d' <"${outfile}" >"${ref_out}"

   ${calc} <"${infile}" | sed -e '1d' >"${new_out}"

   diff "${ref_out}" "${new_out}"

   if [ "$?" -ne 0 ]; then

      echo "FAILED: ${infile}" >&2

      my_exit=1

   fi
   
   /bin/rm "${ref_out}" "${new_out}"

done

exit ${my_exit}
