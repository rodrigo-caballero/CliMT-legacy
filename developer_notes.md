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

## Notes on the existing code

