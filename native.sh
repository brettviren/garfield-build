#!/bin/bash

usage () {
    echo "Error line $1"
    echo "usage: ./native.sh /path/to/work"
    exit 1
}

trap 'usage ${LINENO}' ERR

#set -x
set -e

mydir=$(dirname $(readlink -f $BASH_SOURCE))

outdir=$1 ; shift
if [ ! -d $outdir ] ; then
    mkdir -p $outdir
fi
cd $outdir


download () {
    local url=$1 ; shift
    local fname=$1 
    if [ -z "$fname" ] ; then
	fname=$(basename $url)
    fi
    if [ -f "$fname" ] ; then
	echo "Already downloaded $fname"
	return 0
    fi
    echo "Downloading $fname"
    wget -q -O $fname $url
}

untar () {
    tfile=$1 ; shift
    expect=$1 ; shift
    if [ -d $expect ] ; then
	echo "Already unpacked $tfile"
	return
    fi
    echo "Unpacking $tfile"
    tar -xf $tfile
    chmod -R u+rwx $expect
}

dopatch () {
    local pfile=$1 ; shift
    local afile="${pfile}.applied"
    if [ -f $afile ] ; then
	echo "Patch already applied: $pfile"
	return
    fi
    local plevel=$1
    if [ -n "$plevel" ] ; then
	plevel=0
    fi
    patch -p$plevel < $mydir/$pfile && touch $afile
}

make_patchy () {
    if [ -f patchy_step ] ; then
	echo "Already have patchy_step"
	return
    fi
    local p=$(which ypatchy)
    cat <<EOF > patchy_step
$p - \$1 \$1 - - - - - - - - - :GO 
EOF
    chmod +x patchy_step
}

make_patchy

download http://cern.ch/rjd/Garfield/garfield-7_linux.cra garfield-7.cra
download http://cern.ch/rjd/Garfield/garfadd-7_linux.cra garfadd-7.cra
download http://cern.ch/rjd/Garfield/garfield-8_linux.cra garfield-8.cra
download http://cern.ch/rjd/Garfield/garfadd-8_linux.cra garfadd-8.cra
download http://cern.ch/rjd/Garfield/garfield-9_linux.cra garfield-9.cra
download http://cern.ch/rjd/Garfield/garfadd-9_linux.cra garfadd-9.cra

download http://cern.ch/rjd/Garfield/garfield-7.car
download http://cern.ch/rjd/Garfield/heed101garf.car
download http://cern.ch/rjd/Garfield/magboltz-2.car
download http://cern.ch/rjd/Garfield/magboltz-3.car
download http://cern.ch/rjd/Garfield/magboltz-7.car
download http://nebem.web.cern.ch/nebem/files/neBEMV1.8.13.tgz

download http://cern.ch/rjd/Garfield/interface_amd64_linux26.cra interface.cra

download http://cern.ch/rjd/Garfield/makefile_linux Makefile
download http://cern.ch/garfield/help/garfield.hlp garfield.rawhelp
download http://cern.ch/rjd/Garfield/help_input

untar neBEMV1.8.13.tgz V1.8.13
dopatch neBEMV1.8.13.patch 0
if [ -f V1.8.13/lib/libVector.a ] ; then
    echo "Already built neBEMV1.8.13"
else
    pushd V1.8.13 
    make
    popd
fi


dopatch Makefile.patch 0
if [ ! -d bin ] ; then
    mkdir bin
fi
make garfield-9
ls -l bin


