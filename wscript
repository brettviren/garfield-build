#!/usr/bin/env python
'''Build the GARFIELD simulation.  

This includes the GARFIELD FORTRAN sources after 'ypatchy' has been
run on the cradle/card files. 
'''

import os
import sys
import subprocess
from urllib.request import urlopen
from urllib.error import HTTPError

# nebem_version="V1.8.13"
nebem_version="V1.9.04"
nebem_tar=f'neBEM{nebem_version}.tgz'

def get_from_cern(filename, baseurl="https://nebem.web.cern.ch/nebem/files"):
    '''
    Try to download from file from CERN location
    '''
    if os.path.exists(filename):
        print(f'Already have {filename}, remove to reget')
        return
    url = f'{baseurl}/{filename}'
    print (f'Attempting to retrieve {url}')

    try:
        web = urlopen(url)
    except HTTPError as err:
        print(err)
        print(f'''Failed to download
        {url}
See if you can visit this URL in a browser.
You may need a CERN account or one from a recognized institution.
If successful, place the file in the top level directory next to wscript.
        ''')
        sys.exit(1)
    open(filename, 'wb').write(web.read())


def unpack_tar(tar, expect):
    '''
    Unpack dir unless expect exists
    '''
    if os.path.exists(expect):
        print(f'Not unpacking {tar}, directory exists: {expect}')
        return
    print (f'Unpacking {tar}')
    subprocess.check_call(['tar','-xf',tar])
    if not os.path.exists(expect):
        print(f'Failed to unpack {tar} to make {expect}')
        sys.exit(1)
    subprocess.check_call(f'chmod -R u+rwx {expect}'.split())

def options(opt):
    opt.load('compiler_fc')
    opt.load('compiler_c')

def configure(cfg):
    get_from_cern(nebem_tar)
    unpack_tar(nebem_tar, nebem_version)

    cfg.load('compiler_fc')
    cfg.load('compiler_c')

    cernpath = os.environ['PATH'].split(':') +\
        [os.path.expandvars("$CERN/$CERN_LEVEL/bin"),
         os.path.expandvars("$CERN_ROOT/bin"),
         '/cern/pro/bin'] + os.environ['PATH'].split(':')
    cfg.find_program('cernlib', var='CERNLIBSCRIPT', path_list=cernpath)

    if cfg.env.FC_NAME == 'GFORTRAN':
        cfg.env['FCFLAGS'] = '-fbounds-check -fbackslash'.split()

    cfg.check_cfg(package = 'gsl', args=['--cflags', '--libs'], mandatory=True, uselib_store='GSL')
    #cfg.env.LINKFLAGS_GSL = ['-Wl,-Bstatic']

    cfg.check_fortran()
    cfg.check_fortran_verbose_flag()
    cfg.check_fortran_clib()
    cfg.check_fortran_dummy_main()
    cfg.check_fortran_mangling()

    if 'CERN_ROOT' not in os.environ:
        cr = os.path.dirname(os.path.dirname(cfg.env.CERNLIBSCRIPT[0]))
        os.environ['CERN_ROOT'] = cr
        print(f"setting CERN_ROOT={cr} to help run 'cernlib' script")

    cfg.check_cfg(package='packlib', path='cernlib', uselib_store='CERN',
                  args=['graflib/X11,kernlib,mathlib,packlib,lapack'],
                  errmsg="If this fails, try setting CERN_ROOT")

    # some "cernlib" scripts spew absolute paths which confounds waf's pkg info parser
    linkflags_cern = list()
    lib_cern = list()
    libpath_cern = list()
    for lf  in cfg.env.LINKFLAGS_CERN:
        if lf.startswith('/'):
            p = os.path.dirname(lf)
            if not p in libpath_cern:
                libpath_cern.append(p)
            l = os.path.basename(lf)[3:-2]
            if not l in lib_cern:
                lib_cern.append(l)
        else:
            linkflags_cern.append(lf)
    cfg.env.LINKFLAGS_CERN = linkflags_cern
    cfg.env.LIB_CERN = lib_cern + cfg.env.LIB_CERN
    cfg.env.LIBPATH_CERN = libpath_cern + cfg.env.LIBPATH_CERN


    #clibs = subprocess.Popen([cfg.env.CERNLIBSCRIPT, 'graflib/X11,kernlib,mathlib,packlib'],
    # stdout=subprocess.PIPE).communicate()[0]
    #print 'CERNLIB:', clibs
    #cfg.env.CERNLIBS = clibs.split()
#    print 'CERN:', cfg.env.CERNLIBS
    print ('LIB_CERN:',cfg.env.LIB_CERN)
    print ('LIBPATH_CERN:',cfg.env.LIBPATH_CERN)
    print ('LINKFLAGS_CERN:',cfg.env.LINKFLAGS_CERN)
    print ('LIB_GSL:',cfg.env.LIB_GSL)
    print ('LIBPATH_GSL:',cfg.env.LIBPATH_GSL)
    print ('LINKFLAGS_GSL:',cfg.env.LINKFLAGS_GSL)


    return



def build(bld):
    # fixme: make opt/debug a CLI option

    bld.env.LINKFLAGS_GSL = ['-Wl,-Bstatic']


    # note: garfield and nebem have a cyclic dependency....

    garsrc = bld.path.ant_glob('garfield/*.f')
    bld(features = 'fc fcstlib',
        fcflags = ['-g', '-fbounds-check', '-fbackslash',
                   '-fallow-argument-mismatch'],
        linkflags = ['-std=c99', '-Wl,-Bstatic'],
        source   = garsrc,
        target   = 'garfield')


    nebem_dir = bld.path.find_node(nebem_version)

    nebem_src = nebem_dir.find_node("src")
    

    nebemsrcs = [s for s in nebem_src.ant_glob([
        'NR/*.c',
        'Solve/*.c',
        'Interface/GarfieldInterface.c',
        'Interface/neBEMInterface.c',
        'PreProcess/ReTriM.c',
        'Vector/Vector.c',
    ]) if not s.name.startswith('test')]

    nebem_inc = nebem_dir.find_node("include")
    bld.stlib(target = 'nebemlib', 
              features='c cstlib',
              includes = [nebem_inc] + bld.env.INCLUDES_GSL,
              cflags = ['-g', '-Wall', '-std=c99'],
              source = nebemsrcs)

    # for what in ['ield','add','int']:
    #     bld(features = 'fc fcstlib',
    #         fcflags = ['-O3', '-fbounds-check', '-fbackslash'],
    #         linkflags = ['-std=c99', '-Wl,-Bstatic'],
    #         source   = bld.path.ant_glob('garf%s/*.f'%what),
    #         target   = 'garf'+what)

    nebem_lib = nebem_dir.find_node('lib')

    bld(features = 'fc fcprogram',
        fcflags = ['-g', '-fbounds-check', '-fbackslash',
                   '-fallow-argument-mismatch'],
        includes = [f'{nebem_version}/include'] +bld.env.INCLUDES_GSL,
        source   = ['main.f'], # + garsrc,
        target   = 'garfield-9',
        lib =  [
            'garfield', 'nebemlib', 'garfield',
            'Isles'],
        libpath = [nebem_lib.abspath()],
        linkflags = [
            '-std=c99', '-Wl,-Bstatic', '-no-pie', '-fopenmp',
        ],
        #use  = ['garfadd','garfield','garfint','nebemlib'])
        #use  = ['garfield', 'nebemlib','garfield','GSL','CERN']
        use  = ['nebemlib', 'garfield', 'GSL','CERN']        
    )

