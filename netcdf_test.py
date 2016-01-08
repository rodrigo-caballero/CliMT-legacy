'''
Python code to test reading RRTM absorption data into fortran modules

Do this first to create a shared object that knows about fortran modules:

```
cd /Users/Brian/CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/modules
f2py -c -m rrlw_kg parkind.f90 rrlw_ncpar.f90 rrlw_kg*.f90
```

'''

import numpy as np
import netCDF4 as nc
import rrlw_kg

ncfilepath = '/Users/Brian/CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/data/rrtmg_lw.nc'
data = nc.Dataset(ncfilepath)

def name(bandNumber):
    '''Just creates a string `rrlw_kgxx` where `xx` is a zero-padded version
    of the input number, e.g. `rrlw_kg01` '''
    index = '{:02}'.format(bandNumber)
    thisstr = 'rrlw_kg'
    name = thisstr + index
    return name

# Following code in `CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/src/rrtmg_lw_read_nc.f90`
gPointSetNumber = 1

#  But this number is used as an array size, not an index. Don't modify.
numGPoints = 16
# (actually this number is set in each separately from a constant in
#  corresponding module, e.g. rrlw_kg.rrlw_kg01.no1 etc. But they are all just
#  integer 16)

#  Some dimension constants
#  For some reason the cases got messed up
keylower = rrlw_kg.rrlw_ncpar.keylower
keyupper = rrlw_kg.rrlw_ncpar.keyupper
Tdiff = rrlw_kg.rrlw_ncpar.tdiff
ps = rrlw_kg.rrlw_ncpar.ps
plower = rrlw_kg.rrlw_ncpar.plower
pupper = rrlw_kg.rrlw_ncpar.pupper
Tself = rrlw_kg.rrlw_ncpar.tself
Tforeign = rrlw_kg.rrlw_ncpar.tforeign
T = rrlw_kg.rrlw_ncpar.t
Tplanck = rrlw_kg.rrlw_ncpar.Tplanck
band = rrlw_kg.rrlw_ncpar.band
GPoint = rrlw_kg.rrlw_ncpar.gpoint
GPointSet = rrlw_kg.rrlw_ncpar.gpointset

#  A dictionary mapping the RRTM variable names to the names in .nc file
varname = {'fracrefao': 'PlanckFractionLowerAtmos',
           'fracrefbo': 'PlanckFractionUpperAtmos',
           'kao':       'KeySpeciesAbsorptionCoefficientsLowerAtmos',
           'kbo':       'KeySpeciesAbsorptionCoefficientsUpperAtmos',
           'selfrefo':  'H20SelfAbsorptionCoefficients',
           'forrefo':   'H20ForeignAbsorptionCoefficients',
           'kao_mn2':   'AbsorptionCoefficientsLowerAtmos',
           'kbo_mn2':   'AbsorptionCoefficientsUpperAtmos',
           'kao_mn2o':  'AbsorptionCoefficientsLowerAtmos',
           'kbo_mn2o':  'AbsorptionCoefficientsUpperAtmos',
           'kao_mo3':   'AbsorptionCoefficientsLowerAtmos',
           'ccl4o':     'AbsorptionCoefficientsLowerAtmos',
           'kao_mco2':  'AbsorptionCoefficientsLowerAtmos',
           'kbo_mco2':  'AbsorptionCoefficientsUpperAtmos',
           'cfc11adjo': 'AbsorptionCoefficientsLowerAtmos',
           'cfc12o':    'AbsorptionCoefficientsLowerAtmos',
           'kao_mo2':   'AbsorptionCoefficientsLowerAtmos',
           'kbo_mo2':   'AbsorptionCoefficientsUpperAtmos',
           }

#  A dictionary that holds list of all the names of fields that need to be set for each band
#  Keys are the names of the corresponding subroutines in `rrtmg_lw_read_nc.f90`
fields = {'rrlw_kg01': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'kao_mn2',
                       'kbo_mn2', 'selfrefo', 'forrefo'],
          'rrlw_kg02': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'selfrefo', 'forrefo'],
          'rrlw_kg03': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'kao_mn2o',
                          'kbo_mn2o', 'selfrefo', 'forrefo'],
          'rrlw_kg04': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'selfrefo', 'forrefo'],
          'rrlw_kg05': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'kao_mo3',
                          'selfrefo', 'forrefo', 'ccl4o'],
          'rrlw_kg06': ['fracrefao', 'kao', 'kao_mco2', 'selfrefo', 'forrefo',
                         'cfc11adjo', 'cfc12o'],
          'rrlw_kg07': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'kao_mco2',
                          'kbo_mco2', 'selfrefo', 'forrefo'],
          'rrlw_kg08': ['fracrefao', 'fracrefbo', 'kao', 'kao_mco2', 'kao_mn2o',
                          'kao_mo3', 'kbo', 'kbo_mco2', 'kbo_mn2o', 'selfrefo', 'forrefo',
                          'cfc12o', 'cfc22adjo'],
          'rrlw_kg09': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'kao_mn2o',
                            'kbo_mn2o', 'selfrefo', 'forrefo'],
          'rrlw_kg10': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'selfrefo', 'forrefo'],
          'rrlw_kg11': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'kao_mo2',
                          'kbo_mo2', 'selfrefo', 'forrefo'],
          'rrlw_kg12': ['fracrefao', 'kao', 'selfrefo', 'forrefo'],
          'rrlw_kg13': ['fracrefao', 'fracrefbo', 'kao', 'kao_mco2', 'kao_mco',
                          'kbo_mo3', 'selfrefo', 'forrefo'],
          'rrlw_kg14': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'selfrefo', 'forrefo'],
          'rrlw_kg15': ['fracrefao', 'kao', 'kao_mn2', 'selfrefo', 'forrefo'],
          'rrlw_kg16': ['fracrefao', 'fracrefbo', 'kao', 'kbo', 'selfrefo', 'forrefo'],
         }


for bandNumber in range(1,17):
    try:
        print getattr(rrlw_kg, name(bandNumber)).fracrefbo.shape
    except:
        print 'oheh'

#  Some arrays are all the same size: selfrefo and forrefo
for bandNumber in range(1,17):
    this_name = name(bandNumber)
    this_module = getattr(rrlw_kg, this_name)
    for this_field in fields[this_name]:
        this_var = data.variables[varname[this_field]]
        if this_field == 'selfrefo':
            this_array = this_var[gPointSetNumber-1,bandNumber-1,:numGPoints,:Tself]
        elif this_field == 'forrefo':
            this_array = this_var[gPointSetNumber-1,bandNumber-1,:numGPoints,:Tforeign]
        elif this_field == 'fracrefao':
            if this_module.fracrefao.shape == (16, 9):
                this_array = this_var[gPointSetNumber-1,bandNumber-1,:keylower,:numGPoints].T
            elif this_module.fracrefao.shape == (16,):
                this_array = this_var[gPointSetNumber-1,bandNumber-1,0,:numGPoints].T
        elif this_field ==


        #  Also need to transpose each array to get the right order of dimensions
        setattr(this_module, this_field, this_array.T)


    #  selfrefo
    this_module.selfrefo = data.variables['H20SelfAbsorptionCoefficients'][gPointSetNumber-1,bandNumber-1,:numGPoints,:Tself].T
    #  forrefo
    this_module.forrefo = data.variables['H20ForeignAbsorptionCoefficients'][gPointSetNumber-1,bandNumber-1,:numGPoints,:Tforeign].T
    #  fracrefao
    if this_module.fracrefao.shape == (16, 9):
        this_module.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1,bandNumber-1,:keylower,:numGPoints].T
    elif this_module.fracrefao.shape == (16,):
        this_module.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1,bandNumber-1,0,:numGPoints].T
    else:
        raise ValueError('Something went wrong with the input.')
    #  fracrefbo


####  Earlier...  I was doing it all 'by hand'

# subroutine `lw_kgb01`
bandNumber = 1 - 1
rrlw_kg.rrlw_kg01.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber,bandNumber,0,:numGPoints].T
rrlw_kg.rrlw_kg01.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber,bandNumber,0,:numGPoints].T
rrlw_kg.rrlw_kg01.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber,bandNumber,:numGPoints,:plower,:Tdiff,0].T
rrlw_kg.rrlw_kg01.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber,bandNumber,:numGPoints,:pupper,:Tdiff,0].T
rrlw_kg.rrlw_kg01.selfrefo = data.variables['H20SelfAbsorptionCoefficients'][gPointSetNumber,bandNumber,:numGPoints,:Tself].T
rrlw_kg.rrlw_kg01.forrefo = data.variables['H20ForeignAbsorptionCoefficients'][gPointSetNumber,bandNumber,:numGPoints,:Tforeign].T
#	!Get absorber index for N2
#	call getAbsorberIndex('N2',ab)
ab = rrlw_kg.rrlw_ncpar.getabsorberindex('N2')
ab -= 1  # subtract one to get 0-based index
rrlw_kg.rrlw_kg01.kao_mn2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber,bandNumber,ab,:numGPoints,:T,0].T
rrlw_kg.rrlw_kg01.kbo_mn2 = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber,bandNumber,ab,:numGPoints,:T,0].T


# subroutine `lw_kgb02`
bandNumber = 2 - 1
rrlw_kg.rrlw_kg02.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber,bandNumber,0,:numGPoints].T
rrlw_kg.rrlw_kg02.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber,bandNumber,0,:numGPoints].T
rrlw_kg.rrlw_kg02.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber,bandNumber,:numGPoints,:plower,:Tdiff,0].T
rrlw_kg.rrlw_kg02.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber,bandNumber,:numGPoints,:pupper,:Tdiff,0].T
rrlw_kg.rrlw_kg02.selfrefo = data.variables['H20SelfAbsorptionCoefficients'][gPointSetNumber,bandNumber,:numGPoints,:Tself].T
rrlw_kg.rrlw_kg02.forrefo = data.variables['H20ForeignAbsorptionCoefficients'][gPointSetNumber,bandNumber,:numGPoints,:Tforeign].T



#  subroutine `lw_kgb16`
bandNumber = 16 - 1
# have to switch from fortran indexing to python indexing ... 1 becomes 0.

#  Also need to transpose each array to get the right order of dimensions
rrlw_kg.rrlw_kg16.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber,bandNumber,:keylower,:numGPoints].T
rrlw_kg.rrlw_kg16.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber,bandNumber,0,:numGPoints].T
rrlw_kg.rrlw_kg16.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber,bandNumber,:numGPoints,:plower,:Tdiff,:keylower].T
rrlw_kg.rrlw_kg16.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber,bandNumber,:numGPoints,:pupper,:Tdiff,0].T
rrlw_kg.rrlw_kg16.selfrefo = data.variables['H20SelfAbsorptionCoefficients'][gPointSetNumber,bandNumber,:numGPoints,:Tself].T
rrlw_kg.rrlw_kg16.forrefo = data.variables['H20ForeignAbsorptionCoefficients'][gPointSetNumber,bandNumber,:numGPoints,:Tforeign].T
