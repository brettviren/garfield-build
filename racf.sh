#!/bin/bash

mydir=$(dirname $BASH_SOURCE)

waf () { 
    $mydir/waf-1.7.16 $@
}

# Install GSL by hand
gslroot=/gpfs01/lbne/users/sw/garfield


#export CERN=/afs/rhic.bnl.gov/asis/x8664_sl5/cern64
export CERN=/cern64
export CERN_LEVEL=pro
export PKG_CONFIG_PATH=$gslroot/lib/pkgconfig:$CERN/$CERN_LEVEL/bin
export LD_LIBRARY_PATH=$mydir/build:$gslroot/lib
PATH=$mydir/build:$PATH:$CERN/$CERN_LEVEL/bin:/cern/pro/bin


if [ -n "$1" ] ; then
    $@
fi
