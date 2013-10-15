#!/bin/tcsh -f

pgf90 -DSUN -DPLON=1 -DPLEV=18 -DPLEVR=18 -DSINGLE_SOURCE_FILE -I..  -O -r8 -i4 -Mextend -o crm.x test.crm.F

