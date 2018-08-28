#!/bin/sh
#
# Copyright (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)
#
# wrapper script for configuring ASDL when using the SML/NJ installer
#
# usage:
#	config.sh <install-dir>
#
# where <install-dir> is the root of the SML/NJ installation.
#

if [ x"$INSTALLDIR" = x ] ; then
  echo "config.sh: expected INSTALLDIR to be defined in environment"
  exit 1
fi

SMLNJ_CMD=$INSTALLDIR/bin/sml
export SMLNJ_CMD

./configure --prefix=$INSTALLDIR
