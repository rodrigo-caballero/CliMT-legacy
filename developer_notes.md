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

---------------------------------------
And now I have set things up to call the RRTM initialization subroutine **from Python** upon instantiation of the RRTM radiation object, and commented out the init call in the Driver.f90 subroutine that gets called at every time step. The data are still being read in Fortran code. Next step is to replace the Fortran code with Python code to populate the relevant data modules. Almost there.

And test, of course. Make sure this code actually reproduces results from the old code.

--------------------------------------

Things are now complete... Python code to populate the storage modules, Fortran code to do the rest of the initialization. But unfortunately... it doesn't work. I haven't sorted out why.
But if I include the old Fortran code to read the data, and then do this:
```
import climt
r = climt.radiation(scheme='rrtm')
r.Extension._rrtm_radiation_fortran.rrlw_kg01.fracrefao
```

I do **not** get the same array as we get by reading in the data with Python.
And a test script `forcing_due_to_co2_doubling_rrtm` produces gobbledeegook with my code.

It really looks like the `f2py` wrapper to the storage modules is getting mixed up about data types. But if that's the case, then why aren't there other bugs in the interface? I don't know. Frustrating.

-------------------------

Success! Indeed, it was a problem with custom Fortran data types declared in RRTM code.
The solution is to include a file `.f2py_f2cmap` that tells `f2py` what C data type to use.

--------------------------

Following essentially the same procedure, I have now succeeded in initializing both LW and SW code from Python.

**BUT I AM A BLOODY IDIOT** because I have just now noticed that **RRTM never actually calls the subroutines in `rrtmg_lw_read_nc.f90` and `rrtmg_sw_read_nc.f90`**. It actually just hard-codes the  data in subroutines in `rrtmg_sw_k_g.f90` and `rrtmg_lw_k_g.f90`

So essentially several days of work has been completely unnecessary. Except now we have a working, more modular RRTM code and all I/O occurs through Python, which is at least logically satisfying.

But **the netcdf dependence in CliMT is actually ALL in CAM3 radiation, NOT RRTM!!!!**

Anyway the work is done. I have also created a data directory
`lib/climt/data/rrtm` to hold the two data files `rrtmg_lw.nc` and `rrtmg_sw.nc`. These get copied into the user installation by the `setup.py` install script, just like CAM3 data file. My custom init code now reads from these files instead of hard-coded paths to original files in RRTM code.


# Removing NetCDF dependence from CAM3 radiation code

Now we have to work on CAM3.

The calls to netcdf libraries are in
`src/radiation/cam3/src/radae.F90` in `subroutine radae_init`
This is called from `crm.F90` in `subroutine crm_init_absems`
which is in turn called at object instantiation time by Python code in
`radiation.py` in the `__cam3__init__` method.

So this is a simpler procedure than in the case of RRTM. We want to call Python code that replaces functionality in `subroutine crm_init_absems`  (basically read data and write to appropriate storage). But no need to change the order of operations.

The relevant calls seem to be starting on line 2959 of `radae.F90`
```
call wrap_get_var_realx (ncid_ae, ah2onwid,   ah2onw)
call wrap_get_var_realx (ncid_ae, eh2onwid,   eh2onw)
call wrap_get_var_realx (ncid_ae, ah2owid,    ah2ow)
call wrap_get_var_realx (ncid_ae, cn_ah2owid, cn_ah2ow)
call wrap_get_var_realx (ncid_ae, cn_eh2owid, cn_eh2ow)
call wrap_get_var_realx (ncid_ae, ln_ah2owid, ln_ah2ow)
call wrap_get_var_realx (ncid_ae, ln_eh2owid, ln_eh2ow)
```

Looks straightforward. `wrap_get_var_realx` is just a wrapper for netcdf read that returns doubles.

And in the file `abs_ems_factors_fastvx.c030508.nc'`, each variable in `['ah2onw', 'eh2onw', 'ah2ow', 'ln_ah2ow', 'cn_ah2ow', 'ln_eh2ow', 'cn_eh2ow']`
has the same dimensions: `(7, 21, 25, 10, 10)`

Note that the variable names are the SAME as the names of the storage arrays defined in `radae.F90`

So the code to read them in should be simple. Just need to expose the module variables `['ah2onw', 'eh2onw', 'ah2ow', 'ln_ah2ow', 'cn_ah2ow', 'ln_eh2ow', 'cn_eh2ow']` from `radae.F90` to Python.

Actually I'm taking a slightly different approach to hopefully simplify things.
All we want to expose to python are the storage arrays, not everything in module `radae.F90`.
So I moved the definition of those arrays to a new module called `absems.F90`, which is now used by `radae.F90`. Seems to be working just fine -- though the values are still being set by calls to netcdf within fortran code.
