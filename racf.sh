#!/bin/bash

export CERN=/afs/rhic.bnl.gov/asis/x8664_sl5/cern64
export CERN_LEVEL=pro
export PKG_CONFIG_PATH=/afs/rhic.bnl.gov/opt/astro/SL64/lib/pkgconfig:$CERN/$CERN_LEVEL/bin
PATH=$CERN/$CERN_LEVEL/bin:$PATH

if [ -n "$1" ] ; then
    $@
fi
