#!/usr/bin/env python

import climt,numpy

# Instantiate radiation module 
r = climt.radiation(scheme='ccm3')

# Object can be called like a function, e.g.
r()
# the effect of this call is to
# - set up default T, q, O3 profiles (no clouds by default)
# - evaluate resulting rad fluxes.

# Results are available directly from the object, which can be indexed like a dictionary.
# E.g. this is the OLR:
print r['LwToa'] #  note that all fluxes are defined positive downward, so OLR is negative

# To compute radiative fluxes with non-default paramaters, specify them in input.
# Eg. for 900 ppm CO2:
print ''
r(co2=900.)
print r['LwToa']

# Non-default profiles can also be passed in, but they must be correctly dimensioned.
# The number of grid points in the vertical is specified at compile time as variable KM in setup.py
# and can be retrieved with the get_nlev command:
print ''
nlev = climt.get_nlev()
T = numpy.zeros(nlev) + 273. # isothermal profile; for a more realistic example, see basic_radiation.py
r(T=T)
print r['LwToa']

# You can also pass in an array of profiles; radiative fluxes will be computed for
# each columns in the array:
print ''
T = numpy.array([[T,T],[T,T]]).transpose()
r(T=T)
print r['LwToa']

# Important climt gotcha: each time the object is called, all parameters and fields not explicitly
# passed in are reset to their default values:
print ''
r(co2=900.)
print r['co2']
r()
print r['co2']

# The easiest way to make repeated calls with non-default parameters and fields
# is to define an input dictionary and use it in calls to r():
print ''
input = {}
input['q'] = numpy.zeros(nlev) + 1.e-4
for co2 in 280.*2**numpy.arange(5):
    input['co2'] = co2
    r(**input)
    print r['co2'],r['LwToa']
    

# An alternative, more object-oriented way to get the same result is to set parameter
# and field values directly, and then use the compute() method:
print ''
r['q'] =  numpy.zeros(nlev) + 1.e-4
for co2 in 280.*2**numpy.arange(5):
    r['co2'] = co2
    r.compute() # note that the compute() method does not accept any arguments!
    print r['co2'],r['LwToa']

