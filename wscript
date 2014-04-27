#!/usr/bin/env python
'''
Try to provide a modern version of the absolutely ridiculously archaic
build for GARFIELD:

http://garfield.web.cern.ch/garfield
http://ubuntuforums.org/showthread.php?p=10195676#post10195676

I find it amazing that such a messed up build system is still in use....

'''
garfield_download_url = 'http://cern.ch/rjd/Garfield/'
cradle_files = [
    'garfield-7_linux.cra',
    'garfadd-7_linux.cra',
    'garfield-8_linux.cra',
    'garfadd-8_linux.cra',
    'garfield-9_linux.cra',
    'garfadd-9_linux.cra',
    'interface_amd64_linux26.cra',
]
card_files = [
    'garfield-7.car',
    'heed101garf.car',
    'magboltz-2.car',
    'magboltz-3.car',
    'magboltz-7.car',
]


#    help_input
# http://nebem.web.cern.ch/nebem/files/neBEMV1.8.13.tgz
# http://cern.ch/garfield/help/garfield.hlp

import subprocess

def options(opt):
    opt.load('compiler_fc')
    opt.load('compiler_c')

def configure(cfg):
    cfg.load('compiler_fc')
    cfg.load('compiler_c')
    cfg.find_program('ypatchy', var='YPATCHY')
    cfg.find_program('fcasplit', var='FCASPLIT')

    if cfg.env.FC_NAME == 'GFORTRAN':
        cfg.env['FCFLAGS'] = '-O3 -fbounds-check -fbackslash'.split()


    cfg.check_fortran()
    cfg.check_fortran_verbose_flag()
    cfg.check_fortran_clib()
    cfg.check_fortran_dummy_main()
    cfg.check_fortran_mangling()

    clibs = subprocess.check_output('cernlib graflib/X11,kernlib,mathlib,packlib', shell=True)
    cfg.env.CERNLIBS = clibs
    print cfg.env.CERNLIBS
    return



def build(bld):
    from waflib import Build
    bld.post_mode = Build.POST_LAZY
    fod = bld.path.find_or_declare

    for f in cradle_files + card_files:
        bld(rule = download, target = f, 
            url_base = garfield_download_url, update_outputs = True)
    bld(rule = download, target = 'neBEMV1.8.13.tgz',
        url_base = 'http://nebem.web.cern.ch/nebem/files/', update_outputs = True)

    bld(rule = 'tar -xf ${SRC[0].abspath()} && chmod -R +rwx V1.8.13/src/Devices/Iarocci',
        source = 'neBEMV1.8.13.tgz',
        target = 'V1.8.13/Makefile')

    bemobjs = ['GarfieldInterface.o','neBEMInterface.o','ReTriM.o',
               'ComputeProperties.o','neBEM.o']
    bemobj_paths = ['V1.8.13/obj/'+o for o in bemobjs]

    # Build neBEM
    print bemobj_paths
    bld(rule = 'cd V1.8.13 && make > make.log 2>&1',
        source = 'V1.8.13/Makefile',
        target = bemobj_paths)

    # Rename linux-specific cradle files
    # fixme: handle this during download
    for n in range(7,10,1):
        for what in ['garfield','garfadd']:
            bld(rule='cp ${SRC} ${TGT}',
                source = fod('%s-%d_linux.cra' % (what, n)),
                target = fod('%s-%d.cra' % (what, n)))

    #patchy_split(bld, 'garfield-9.cra garfield-7.car heed101garf.car magboltz-7.car')
    patchy_split(bld, 'garfadd-9.cra garfield-7.car heed101garf.car magboltz-7.car')

    bld.add_group()

    bld(features = 'find_source fc fcstlib', target = 'garfadd-9', source_glob='*.f')

    # linkflags = bemobj_paths + bld.env.CERNLIBS.split()
    # print 'LINKFLAGS',linkflags
    # bld(features = 'fc fcprogram',
    #     target   = 'garfield-9',
    #     use = ['garfield-9.o','garfadd-9.o'],
    #     stlibpath = 'V1.8.13/lib',
    #     stlib = ['NR', 'Vector', 'Isles', 'gslcblas', 'gsl', 'm'],
    #     linkflags = linkflags)
    return

# some helpers below

from waflib.TaskGen import feature, before
@feature('find_source')
@before('process_source')
def find_generated_source_files(self):
    work = self.path.get_bld().find_node('lib'+to_list(self.target)[0])
    self.source = to_list(self.source) + work.ant_glob(self.source_glob)


from waflib.Configure import urlopen
def download(task):
    gen = task.generator
    #bld = gen.bld
    tgt = task.outputs[0]
    url = gen.url_base + str(tgt)
    #print 'TASK:',gen,sorted(gen.__dict__.keys())
    print 'Downloading %s' % url
    web = urlopen(url)
    assert web.getcode() == 200
    tgt.write(web.read(), 'wb')



from waflib.Utils import to_list
from waflib.Task import compile_fun

def fcasplit(task):
    cmd_fun, _ = compile_fun("${FCASPLIT} ${SRC[0].abspath()}")
    rc = cmd_fun(task)
    if rc: return rc
    cwd = task.generator.bld.root.make_node(task.cwd)
    task.outputs = cwd.ant_glob('*.f')


def patchy_split(bld, source):
    source = to_list(source)
    name = source[0].replace('.cra','')

    fortnode = bld.path.find_or_declare(name+'.f')
        
    cmd = '${YPATCHY} - %s %s - - - - - - - - - :GO > patchy.log|| true' % (name,name)
    bld(rule = cmd, source = source, target = fortnode)

    work = bld.path.get_bld().make_node('lib'+name)
    work.mkdir()
    split_out = work.find_or_declare(name+'.mkfca')

    bld(rule = fcasplit,
        source = fortnode, target = split_out, cwd = work.abspath())
        
    # bld(rule = generated_fclib,
    #     source = split_out, 
    #     libname = name, update_outputs=True)





    

# import os
# from waflib.Utils import to_list

# def generated_fclib(task):
#     gen = task.generator
#     bld = gen.bld
#     work = task.inputs[0].parent
#     bld(features = 'fc fcstlib',
#         source = work.ant_glob('*.f'),
#         target = gen.libname)



# # http://stackoverflow.com/questions/8505588/how-to-compile-the-c-source-files-generated-in-run-time-using-waf
# import waflib.TaskGen
# @waflib.TaskGen.feature('fc')
# @waflib.TaskGen.before('process_source')
# def dynamic_post(self):
#     if not getattr(self, 'dynamic_source', None):
#         return
#     self.source = to_list(self.source)
#     self.source.extend(self.path.get_bld().ant_glob(self.dynamic_source))


# from waflib import TaskGen
# TaskGen.declare_chain(
#         name         = 'patchy', 
#         rule         = '${PATCHY} -- ${SRC[0]}} .go', 
#         shell        = False,
#         ext_in       = '.', 
#         ext_out      = '.luac', 
#         reentrant    = False, 
#         install_path = '${LUADIR}', 
# )





# def patchy(task):
#     name = str(task.inputs[0]).replace('.cra','')
#     print ('Force a successful run of nypatchy on %s' % name)
#     cmd = 'nypatchy - %s %s - - - - - - - - - :GO > patchy-%s.log|| true' % \
#           (name,name, name)
#     return task.exec_command(cmd)
