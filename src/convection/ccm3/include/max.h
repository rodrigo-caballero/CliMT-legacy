#ifndef MAX_H
#define MAX_H

#define MAX_PATH_LEN       128
#define MAX_LEVELS         100
#define MAX_FIELDNAME_LEN  64
#define MAX_FIELDS         500
#define MAX_DATASET_LEVS   100  
#define MAX_TIME_DIM     50000 
#define MAX_RECORDS         12    
#define MAX_LON_DIM        128
#define MAX_LAT_DIM         64

#define NUM_USER_SWITCHES   12
#define NUM_OPTION_SWITCHES  5

#ifdef __COMMENTS__
  the following #defines must be single constants because rpcgen
  is too stupid to handle things like "float x[(12+2)];"
#endif  /* __COMMENTS__ */

#define NUM_SWITCHES         17 /* (NUM_USER_SWITCHES + NUM_OPTION_SWITCHES) */
#define SRFPROP_SW           12 /*  (0 + NUM_USER_SWITCHES) */
#define RELAX_SW             13 /*  (1 + NUM_USER_SWITCHES) */
#define FRC3D_SW             14 /*  (2 + NUM_USER_SWITCHES) */
#define PERT_INIT_SW         15 /*  (3 + NUM_USER_SWITCHES) */
#define PERT_FRC_SW          16 /*  (4 + NUM_USER_SWITCHES) */

#endif /* MAX_H */
