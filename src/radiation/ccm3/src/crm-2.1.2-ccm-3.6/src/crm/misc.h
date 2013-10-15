c     $Header: /Users/rca/cvsroot/CliMT/src/radiation/ccm3/src/crm-2.1.2-ccm-3.6/src/crm/misc.h,v 1.1 2004/09/07 15:27:30 rca Exp $ -*-fortran-*-

#ifndef MISC_SET
#define MISC_SET
#define NCPREC NF_FLOAT
#define PVP 
#ifdef CRAY
#define REALTYPE MPI_REAL
#undef  SHELL_MSS
#undef  FORTFFT
#else /* not CRAY */
#define REALTYPE MPI_DOUBLE_PRECISION
#define SHELL_MSS
#define FORTFFT
#endif /* not CRAY */
#undef  COUP_SOM
#undef  COUP_CSM
#undef  SPMD
#if ( ! defined CRAY ) && ( ! defined SUN )  && ( ! defined SGI )
You must define one of CRAY, SUN or SGI 
#endif /* not CCM_ARCH */ 
#endif /* not MISC_SET */


