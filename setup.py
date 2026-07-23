
# -------- EDIT HERE TO MATCH YOUR ENVIRONMENT:
KM = 26
JM = 1
IM = 1
NC_INC = '/Users/rca/miniconda3/envs/py312/include'
NC_LIB = '/Users/rca/miniconda3/envs/py312/lib'
# --------------------------------------------------------

from setuptools import setup, Extension 
import os,glob,sys,string
from pathlib import Path

# the following 2 lines are for OSX only!!
# comment out if you are using Linux
os.environ['C_INCLUDE_PATH'] = '/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include'
os.environ['CC'] = 'clang'

if '--lite' in sys.argv:
    sys.argv.pop(sys.argv.index('--lite'))
    Lite = True
else:
    Lite = False

climt_home_dir = str(Path(__file__).parent) + '/'
Extensions = [
    {'name':'grid',
     'dir':climt_home_dir+'src/grid'},
    {'name':'timestep',
     'dir':climt_home_dir+'src/timestep'},
    {'name':'thermodyn',
     'dir':climt_home_dir+'src/thermodyn'},
    {'name':'emanuel_convection',
     'dir':climt_home_dir+'src/convection/emanuel'},
    {'name':'hard_adjustment',
     'dir':climt_home_dir+'src/convection/hard'},
    #{'name':'sbm_convection',
    # 'dir':climt_home_dir+'src/convection/sbm'},
    {'name':'axisymmetric_dynamics',
     'dir':climt_home_dir+'src/dynamics/axisymmetric'},
    {'name':'two_column_dynamics',
     'dir':climt_home_dir+'src/dynamics/two_column'},
    {'name':'slab_ocean',
     'dir':climt_home_dir+'src/ocean/slab_ocean'},
    {'name':'ccm3_radiation',
     'dir':climt_home_dir+'src/radiation/ccm3',
     'cppflags':'-DSUN -DPLON=%i -DPLEV=%i -DPLEVR=%i' % (IM,KM,KM)},
    {'name':'cam3_radiation',
     'dir':climt_home_dir+'src/radiation/cam3',
     'cppflags':'-DPLEV=%i' % KM,
     'lib':['netcdf','netcdff'],
     'libdir': [NC_LIB],
     'incdir': [NC_INC]},
    {'name':'chou_radiation',
     'dir':climt_home_dir+'src/radiation/chou'},
    {'name':'greygas_radiation',
     'dir':climt_home_dir+'src/radiation/greygas'},
    #{'name':'rrtm_radiation_fortran',
    # 'dir':climt_home_dir+'src/radiation/rrtm'}
    {'name':'ozone',
     'dir':climt_home_dir+'src/radiation/ozone'},
    {'name':'insolation',
     'dir':climt_home_dir+'src/radiation/insolation'},
    {'name':'ccm3_turbulence',
     'dir':climt_home_dir+'src/turbulence/ccm3',
     'cppflags':'-DPLON=%i -DPLEV=%i' % (IM,KM)},
    {'name':'simple_turbulence',
     'dir':climt_home_dir+'src/turbulence/simple'}
    ]

# define extensions that will be built when the --lite option is used
LiteExtensionsNames = ['grid','timestep','insolation','ozone','thermodyn','ccm3_radiation']
ExtensionsLite = []
for ext in Extensions:
    if ext['name'] in LiteExtensionsNames:
        ExtensionsLite.append(ext)

# set some gfortran compiler flags
f77flags='-ffixed-line-length-132 -fdefault-real-8 -std=legacy'
f90flags='-fdefault-real-8 -std=legacy'

for ExtList in [Extensions,ExtensionsLite]:
    for i in range(len(ExtList)):
        Defaults = {'cppflags':'-DIM=%i -DJM=%i -DKM=%i' % (IM,JM,KM),
                    'f77flags':f77flags,
                    'f90flags':f90flags}
        Defaults.update(ExtList[i])
        ExtList[i] = Defaults

def getSources(dir):
    #Gets list of source files for extensions
    # -- some modules (eg. cam3) have a specific list of source files
    if os.path.exists(dir+'/sources_in_order_of_compilation'):
        Sources = open(dir+'/sources_in_order_of_compilation').readlines()
        Sources = [os.path.join(dir,s[:-1]) for s in Sources]
    # -- if no specific list, then just get all fortran files
    else:
        Sources = glob.glob(dir+'/**', recursive=True)
        Sources = [file for file in Sources if ('.f90' in file) or ('.F90' in file) \
                   or ('.f' in file) or ('.F' in file)]
        Sources = [file for file in Sources if ('ignore' not in file)]
    return Sources

def buildNeeded(name, src):
    #Checks if source code is newer than extension, so extension needs to be rebuilt
    target = glob.glob('lib/climt/_%s*.so' % name)
    if len(target) == 0:
        return True
    else:
        target = target[0]
    for file in src:
        if os.path.getctime(file) > os.path.getctime(target):
            return True
    print('Extension %s is up to date' % os.path.basename(target))
    return False

def build_ext(name=None, dir=None, cppflags='', f77flags='', f90flags='', \
              lib='', libdir='', incdir=''):
    #Builds an extension
    f77flags = '"%s %s"' % (cppflags,f77flags)
    f90flags = '"%s %s"' % (cppflags,f90flags)
    src = getSources(dir)
    if buildNeeded(name, src):
        print('\n Building %s ... \n' % name)
        # generate signature file for Driver only
        # (need to do this because otherwise f2py tries to generate signatures for all files,
        # and runs into problems with preprocessing)
        driver =  glob.glob(os.path.join(dir, 'Driver.f*'))[0]
        os.system('f2py --overwrite-signature %s -m _%s -h _%s.pyf' % (driver, name, name))
        # compile extension
        F2pyCommand = []
        F2pyCommand.append('f2py --backend meson -c -m _%s' % name)
        F2pyCommand.append('-I%s' % dir)
        F2pyCommand.append('-I%s' % os.path.join(dir,'src'))
        if incdir != '':
            for i in incdir:
                F2pyCommand.append('-I%s' % i)
        if libdir != '':
            for i in libdir:
                F2pyCommand.append('-L%s' % i)
        if lib != '':
            for i in lib:
                F2pyCommand.append('-l%s' % i)
        F2pyCommand.append('--f77flags=%s' % f77flags)
        F2pyCommand.append('--f90flags=%s' % f90flags)
        F2pyCommand.append('_%s.pyf' % name)
        F2pyCommand.append(' '.join(src))
        F2pyCommand = ' '.join(F2pyCommand)
        print(F2pyCommand)
        if os.system(F2pyCommand) > 0:
            print('+++ Compilation failed')
            sys.exit()
        os.system('mv -f _%s*.so lib/climt' % name)
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
    print(DataFiles)
    os.chdir('../..')
    
    setup(name = "CliMT-legacy",
          version = open('Version').read()[:-1],
          description = "Climate modelling and diagnostics toolkit",
          author = "Rodrigo Caballero",
          author_email = "rodrigo@misu.su.se",
          url = "http://people.su.se/~rcaba/climt",
          packages = ['climt_legacy'],
          package_dir = {'climt_legacy':'lib/climt'},
          package_data = {'climt_legacy':['*.so']+DataFiles})


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
