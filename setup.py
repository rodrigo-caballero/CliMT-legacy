#!/usr/bin/env python

import os,glob,string,sys
from numpy.distutils.core import setup, Extension
from numpy.distutils import fcompiler
from distutils.dep_util import newer

## -------- set these
KM = 26
JM = 1
IM = 1
#NC_INC = '/usr/local/include'
#NC_LIB = '/usr/local/lib'
##----------------------

if '--lite' in sys.argv:
    sys.argv.pop(sys.argv.index('--lite'))
    Lite = True
else:
    Lite = False

Extensions = [
    {'name':'grid',
     'dir':'src/grid'},
    {'name':'timestep',
     'dir':'src/timestep'},
    {'name':'thermodyn',
     'dir':'src/thermodyn'},
    {'name':'emanuel_convection',
     'dir':'src/convection/emanuel'},
    {'name':'hard_adjustment',
     'dir':'src/convection/hard'},
    {'name':'sbm_convection',
     'dir':'src/convection/sbm'},
    {'name':'axisymmetric_dynamics',
     'dir':'src/dynamics/axisymmetric'},
    {'name':'two_column_dynamics',
     'dir':'src/dynamics/two_column'},
    {'name':'slab_ocean',
     'dir':'src/ocean/slab_ocean'},
    {'name':'ccm3_radiation',
     'dir':'src/radiation/ccm3',
     'cppflags':'-DSUN -DPLON=%i -DPLEV=%i -DPLEVR=%i' % (IM,KM,KM)},
    #{'name':'cam3_radiation',
    # 'dir':'src/radiation/cam3',
    # 'cppflags':'-DPLEV=%i' % KM,
    # 'lib':['netcdf','netcdff'],
    # 'libdir': [NC_LIB],
    # 'incdir': [NC_INC]},
    {'name':'chou_radiation',
     'dir':'src/radiation/chou'},
    {'name':'greygas_radiation',
     'dir':'src/radiation/greygas'},
    {'name':'ozone',
     'dir':'src/radiation/ozone'},
    {'name':'insolation',
     'dir':'src/radiation/insolation'},
    {'name':'ccm3_turbulence',
     'dir':'src/turbulence/ccm3',
     'cppflags':'-DPLON=%i -DPLEV=%i' % (IM,KM)},
    {'name':'simple_turbulence',
     'dir':'src/turbulence/simple'},
    {'name':'rrtm_radiation_fortran',
     'dir':'src/radiation/rrtm'}
    ]

# define extensions that will be built when the --lite option is used
LiteExtensionsNames = ['grid','timestep','insolation','ozone','thermodyn','ccm3_radiation']
ExtensionsLite = []
for ext in Extensions:
    if ext['name'] in LiteExtensionsNames:
        ExtensionsLite.append(ext)

# figure out which compiler we're goint to use
compiler = fcompiler.get_default_fcompiler()
for i in range(len(sys.argv)):
    if '--fcompiler' in sys.argv[i]:
        compiler = sys.argv.pop(i)
        compiler = compiler[compiler.index('=')+1:]
print 'Using %s compiler' % compiler

# set some fortran compiler-dependent flags
if compiler == 'gnu95':
    f77flags='-ffixed-line-length-132 -fdefault-real-8'
    f90flags='-fdefault-real-8 -fno-range-check -ffree-form'
elif compiler == 'intel' or compiler == 'intelem':
    f77flags='-132 -r8'
    f90flags='-132 -r8'
elif compiler == 'ibm':
    f77flags='-qautodbl=dbl4 -qsuffix=f=f:cpp=F -qfixed=132'
    f90flags='-qautodbl=dbl4 -qsuffix=f=f90:cpp=F90 -qfree=f90'
else:
    print 'Sorry, compiler %s not supported' % compiler

for ExtList in [Extensions,ExtensionsLite]:
    for i in range(len(ExtList)):
        Defaults = {'cppflags':'-DIM=%i -DJM=%i -DKM=%i' % (IM,JM,KM),
                    'f77flags':f77flags,
                    'f90flags':f90flags}
        Defaults.update(ExtList[i])
        ExtList[i] = Defaults
    if compiler == 'ibm':
        for ext in ExtList:
            ext['cppflags']='-WF,'+string.join(ext['cppflags'].split(),',')

def getSources(dir, source_file_name='sources_in_order_of_compilation'):
    #Gets list of source files for extensions
    SrcFile = os.path.join(dir, source_file_name)
    if os.path.exists(SrcFile):
        Sources = open(SrcFile).readlines()
        Sources = [os.path.join(dir,s[:-1]) for s in Sources]
    else:
        Sources = []
        w = os.walk(dir)
        for ww in w:
            if 'ignore' not in ww[0]:
                for pattern in ['*.f','*.F','*.f90','*.F90']:
                    Sources += glob.glob(os.path.join(ww[0],pattern))
    return Sources

def buildNeeded(target,src):
    #Checks if source code is newer than extension, so extension needs to be rebuilt
    target = os.path.join('lib/climt',target)
    if not os.path.exists(target):
        return True
    for file in src:
        if newer(file,target):
            return True
    print 'Extension %s is up to date' % os.path.basename(target)
    return False

def build_ext(name=None, dir=None, cppflags='', f77flags='', f90flags='', \
              lib='', libdir='', incdir=''):
    #Builds an extension
    src = getSources(dir)
    target = '_%s.so' % name
    print dir
    print glob.glob(os.path.join(dir,'Driver.f*'))
    driver = glob.glob(os.path.join(dir,'Driver.f*'))[0]
    f77flags = '"%s %s"' % (cppflags,f77flags)
    f90flags = '"%s %s"' % (cppflags,f90flags)
    if buildNeeded(target,src):
        print '\n Building %s ... \n' % os.path.basename(target)
        # generate signature file
        if name == 'rrtm_radiation_fortran':
            print 'Adding extra RRTM modules to the signature file to enable Python-level access.'
            src_pyf = getSources(dir, source_file_name='sources_signature_file')
            os.system('f2py --overwrite-signature %s -m _%s -h _%s.pyf'%(string.join(src_pyf),name,name))
        else:
            os.system('f2py --overwrite-signature %s -m _%s -h _%s.pyf'%(driver,name,name))
        # compile extension
        F2pyCommand = []
        F2pyCommand.append('f2py -c -m _%s' % name)
        F2pyCommand.append('--fcompiler=%s --noopt' % compiler)
        F2pyCommand.append('-I%s' % dir)
        F2pyCommand.append('-I%s' % os.path.join(dir,'include'))
        F2pyCommand.append('-I%s' % os.path.join(dir,'src'))
        F2pyCommand.append('-I%s' % os.path.join(dir,'src','include'))
        if incdir is not '':
            for i in incdir:
                F2pyCommand.append('-I%s' % i)
        if libdir is not '':
            for i in libdir:
                F2pyCommand.append('-L%s' % i)
        if lib is not '':
            for i in lib:
                F2pyCommand.append('-l%s' % i)
        F2pyCommand.append('--f77flags=%s' % f77flags)
        F2pyCommand.append('--f90flags=%s' % f90flags)
        F2pyCommand.append('_%s.pyf' % name)
        F2pyCommand.append('%s' % string.join(src))
        F2pyCommand = string.join(F2pyCommand)
        print F2pyCommand
        if os.system(F2pyCommand) > 0:
            print '+++ Compilation failed'
            sys.exit()
        os.system('mv -f _%s.so lib/climt' % name)
        os.system('rm -f _%s.pyf' % name)

def setupClimt():

    # Build all extensions
    for ext in Extensions: build_ext(**ext)

    # Finish the setup
    # note: setup() cannot copy directories, and falls over
    # trying to copy the CVS directory in climt/lib/data
    # workaround: make data list which specifically excludes CVS
    os.chdir('lib/climt')
    DataFiles = []
    for File in glob.glob('data/*/*'):
        if 'CVS' not in File:
            DataFiles.append(File)
    print DataFiles
    os.chdir('../..')

    setup(name = "CliMT",
          version = open('Version').read()[:-1],
          description = "Climate modelling and diagnostics toolkit",
          author = "Rodrigo Caballero",
          author_email = "rodrigo@misu.su.se",
          url = "http://people.su.se/~rcaba/climt",
          packages = ['climt'],
          package_dir = {'':'lib'},
          package_data = {'climt':['*.so']+DataFiles})


def setupClimtLite():
    # Build all extensions
    for ext in ExtensionsLite: build_ext(**ext)
    os.system('mkdir -p lib/climt_lite')
    ClimtLiteFiles = ['__init__.py', '__version__.py', '_ccm3_radiation.so', '_grid.so',
     '_insolation.so', '_ozone.so', '_thermodyn.so', '_timestep.so', 'component.py',
     'grid.py', 'insolation.py', 'mathutil.py', 'ozone.py', 'parameters.py',
     'radiation.py', 'state.py', 'thermodyn.py', 'utils.py', 'io.py', 'plot.py']
    for file in ClimtLiteFiles:
        os.system('cp lib/climt/%s lib/climt_lite/%s' % (file,file))
    setup(name         = "CliMT-lite",
          version      = open('Version').read()[:-1],
          description  = "Climate modelling and diagnostics toolkit, lite version",
          author       = "Rodrigo Caballero",
          author_email = "rodrigo@misu.su.se",
          url          = "http://people.su.se/~rcaba/climt",
          packages    = ['climt_lite'],
          package_dir = {'climt_lite':'lib/climt_lite'},
          package_data = {'climt_lite':['*.so']})

if Lite:
    setupClimtLite()
else:
    setupClimt()
