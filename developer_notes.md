# Notes on eliminating dependence on external netcdf libraries for `CliMT`

## Brian Rose

### The issue

Currently `CliMT` must be compiled against the `netcdf` Fortran libraries. This is a significant barrier to getting `CliMT` up and running on a new machine -- it is often a multi-step process of obtaining the main `netcdf` libraries, compiling them, then compiling the Fortran libraries, then building `CliMT`.

This is not practical for all users, especially students who might want to use `CliMT` or some other interface to the radiation code but are not skilled at command-line development tools.

So my take is to figure out how to remove this dependence from `CliMT`. `CliMT` does not actually use the compiled `netcdf` libraries for its own i/o. The dependence is only in the radiation modules, and specifically for the these two modules:

- CAM3 radiation
- RRTM radiation

In both cases, the underlying Fortran code uses calls to `netcdf` to load absorptivity data from a `.nc` file. That is the *only* part of the `CliMT` code that actually depends on `netcdf` at the Fortran level.

### A better solution

All the i/o should occur at the Python level, and use the standard Python library `netCDF4`. This way the Fortran code would be much more portable. Users would still need to have a Fortran compiler, but would not have to deal at all with run-time linking issues.

Reading in the absorptivity data at the Python level is trivial. The trick will be to package it and pass it to the Fortran code in the same way that the code currently provides for itself.

### The plan

I will focus on the RRTM scheme. Presumably it will be straightforward to implement the same solution for CAM3 scheme later, but most users will be more interested in the more up-to-date RRTM code.

## Notes on the existing code: LONGWAVE

Let's just note where the code actually calls `netcdf` library routines, and work out from there.

The file `CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/src/rrtmg_lw_read_nc.f90`
seems to do all the calls to `netcdf` in the longwave code.

It contains these 16 subroutines (`grep "subroutine" rrtmg_lw_read_nc.f90 | grep -v "end"`)

```
subroutine lw_kgb01
subroutine lw_kgb02
subroutine lw_kgb03
subroutine lw_kgb04
subroutine lw_kgb05
subroutine lw_kgb06
subroutine lw_kgb07
subroutine lw_kgb08
subroutine lw_kgb09
subroutine lw_kgb10
subroutine lw_kgb11
subroutine lw_kgb12
subroutine lw_kgb13
subroutine lw_kgb14
subroutine lw_kgb15
subroutine lw_kgb16
```

Each subroutine loads some modules in which the storage is declared. For example, the last `subroutine lw_kgb16` has

```
use rrlw_kg16, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo, no16
use rrlw_ncpar
```

The module ``rrlw_kg16`` is defined in `CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/modules/rrlw_kg16.f90`

What we need to do is:

- Write python code to read the data file into a `netCDF4.Dataset` object
- Split the data up into individual strings, arrays, etc following exactly what occurs in `rrtmg_lw_read_nc.f90`
- Figure out how to pass all these arrays from Python level to Fortran.

Is there a way to do this once at import time and leave all the data is some kind of Fortran shared object that modules can link to dynamically? Well, just follow the logic that the existing Fortran code actually uses.

What code actually calls all the subroutines in `rrtmg_lw_read_nc.f90`?

The calls are in `CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/src/rrtmg_lw_init.f90` in `subroutine rrtmg_lw_ini`

The driver for the whole RRTMG_LW code is `CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/src/rrtmg_lw_rad.f90`. In this code, the call to `rrtmg_lw_init` is commented out! The comment says

```
! *** Move the required call to rrtmg_lw_ini below and the following
! use association to the GCM initialization area ***
!      use rrtmg_lw_init, only: rrtmg_lw_ini
```
and later

```
! NOTE: The call to RRTMG_LW_INI should be moved to the GCM initialization
!  area, since this has to be called only once.
```

So the question is, when does `CliMT` actually call it?

Answer: in `CliMT/src/radiation/rrtm/Driver.f90`, line 129. Just before the actual calls to `rrtmg_sw` and `rrtmg_lw` (i.e. where the underlying RRTM code is actually called).

What if we get rid of this, but instead have a stand-alone object that we can just import that already has all the fortran modules properly populated with their data?

Do this:

```
cd /Users/Brian/CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/modules
f2py -c -m rrlw_kg parkind.f90 rrlw_kg*.f90
```
which creates a file `rrlw_kg.so`. This is an object that is importable from Python that gives access to the fortran modules that store the data.

_______________

Okay so I have written a python script `netcdf_test.py` that loads that fortran shared object and populates it with the data from the netcdf file in (I think) exactly the same way as the fortran code in `rrtmg_lw_read_nc.f90` does.  The question now is, can I make sure that this data gets passed to the RRTM code at execution time?

------------------

I think it's straightforward. The fortran modules get initialized in memory when the radiation objection is instantiated by a Python call to climt.

Currently the logic is that the Python driver calls the Fortran code at every timestep by a call to `_rrtm_radiation_fortran.driver` in `_rrtm_radiation.py` (which is the Python wrapper for the Fortran `Driver.f90` code.

So in effect the initialization of the absorptivity is occuring **at every timestep**.

All we should need to do is put Python code in
...
and remove the call to initialization code in `Driver.f90`. There should be no need to modify the timestepping code passing values back and forth from Python to fortran modules.

__________________________________
It's a bit more complicated.

What happens in the call to `f2py` in `setup.py` is that a signature file is generated for just `Driver.f90`. So when you import the resulting fortran object in a python session, e.g.

```
import climt
r = climt.radiation(scheme='rrtm')
r.Extension._rrtm_radiation_fortran.driver
```
shows the `<fortran object>` which is the compiled code in `Driver.f90` for the RRTM module. But you do **not** have interactive access to any of the modules contained within it.

I **think** what we need to to is modify the `f2py` calls in `setup.py` (just for RRTM scheme) and add the list `rrlw_kg*.f90` to the call to generate a signature `*.pyf` file. Then these modules should be visible from Python, like in my example above.

-------------------------------------

Ok, I did this. In hacked form. Need to fix up the setup.py code to handle file paths more intelligently, and then ...  set up Python code to initialize the data, and get rid of calls to equivalent Fortran code.
