#!/usr/bin/env python
'''Build the GARFIELD simulation.  

This includes the GARFIELD FORTRAN sources after 'ypatchy' has been
run on the cradle/card files. 
'''

import os
import subprocess

nebem_url = 'http://nebem.web.cern.ch/nebem/files/neBEMV1.8.13.tgz'
from waflib.Configure import urlopen
if not os.path.exists('V1.8.13'):
    print 'Preparing neBEM source'
    fname = os.path.basename(nebem_url)
    if not os.path.exists(fname):
        web = urlopen(nebem_url)
        assert web.getcode() == 200
        open(fname, 'wb').write(web.read())
    subprocess.check_call(['tar','-xf',fname])
    subprocess.check_call('chmod -R u+rwx V1.8.13'.split())


def options(opt):
    opt.load('compiler_fc')
    opt.load('compiler_c')

def configure(cfg):
    cfg.load('compiler_fc')
    cfg.load('compiler_c')

    cernpath = os.environ['PATH'].split(':') +\
        [os.path.expandvars("$CERN/$CERN_LEVEL/bin"), '/cern/pro/bin']
    #cfg.find_program('cernlib', var='CERNLIBSCRIPT', path_list=cernpath)

    if cfg.env.FC_NAME == 'GFORTRAN':
        cfg.env['FCFLAGS'] = '-O3 -fbounds-check -fbackslash'.split()

    cfg.check_cfg(package = 'gsl', args=['--cflags', '--libs'])

    cfg.check_fortran()
    cfg.check_fortran_verbose_flag()
    cfg.check_fortran_clib()
    cfg.check_fortran_dummy_main()
    cfg.check_fortran_mangling()

    cfg.check_cfg(package = '', path='cernlib', uselib_store='CERN',
                  args=['graflib/X11,kernlib,mathlib,packlib,lapack'])

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
    print 'LIB_CERN:',cfg.env.LIB_CERN
    print 'LIBPATH_CERN:',cfg.env.LIBPATH_CERN
    print 'LINKFLAGS_CERN:',cfg.env.LINKFLAGS_CERN
    print 'LIB_GSL:',cfg.env.LIB_GSL
    print 'LIBPATH_GSL:',cfg.env.LIBPATH_GSL
    print 'LINKFLAGS_GSL:',cfg.env.LINKFLAGS_GSL


    return



def build(bld):

    bld.shlib(target = 'nebemlib', 
              includes = ['V1.8.13/include'] +bld.env.INCLUDES_GSL,
              cflags = ['-Wall', '-std=c99', '-O3'],
              source = bld.path.ant_glob([
                  'V1.8.13/src/NR/*.c',
                  'V1.8.13/src/Solve/*.c',
                  'V1.8.13/src/Interface/GarfieldInterface.c',
                  'V1.8.13/src/Interface/neBEMInterface.c',
                  'V1.8.13/src/PreProcess/ReTriM.c',
                  'V1.8.13/src/Vector/Vector.c',
                  ]))


    # for what in ['ield','add','int']:
    #     bld(features = 'fc fcstlib',
    #         fcflags = ['-O3', '-fbounds-check', '-fbackslash'],
    #         linkflags = ['-std=c99', '-Wl,-Bstatic'],
    #         source   = bld.path.ant_glob('garf%s/*.f'%what),
    #         target   = 'garf'+what)

    bld(features = 'fc fcshlib',
        fcflags = ['-O3', '-fbounds-check', '-fbackslash'],
        linkflags = ['-std=c99', '-Wl,-Bstatic'],
        source   = bld.path.ant_glob('garfield/*.f'),
        target   = 'garfield')

    isles_lib = bld.path.find_node('V1.8.13/lib')
    bld(features = 'fc fcprogram',
        fcflags = ['-O3', '-fbounds-check', '-fbackslash'],
        source   = 'main.f',
        target   = 'garfield-9',
        lib = bld.env.LIB_GSL + bld.env.LIB_CERN + ['Isles'],
        libpath = bld.env.LIBPATH_GSL + bld.env.LIBPATH_CERN + [isles_lib.abspath()],
        linkflags = ['-std=c99','-Wl,-Bstatic'],
        #use  = ['garfadd','garfield','garfint','nebemlib'])
        use  = ['garfield','nebemlib'])
