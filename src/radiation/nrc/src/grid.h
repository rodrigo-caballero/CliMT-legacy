C####################################################################
C                     grid.h
C                     March 2002  c-grid
C                     gcm1.7.2 - Aug 2002
C                     GCM2.0  Sept 2002
C
C                     FORTRAN PARAMETERS
C####################################################################

      INTEGER L_JSIZE, L_ISIZE, L_LAYERS, L_LEVELS

C     X dimension of grid arrays
      PARAMETER (L_ISIZE   = 40)

C     Y dimension of grid arrays
      PARAMETER (L_JSIZE   = 24)

C     Number of atmospheric layers
      PARAMETER (L_LAYERS  = 24)

C     Number of atmospheric levels:   2 * L_LAYERS + 3
      PARAMETER (L_LEVELS  = 2*L_LAYERS+3)

      integer L_J, L_I
 
      PARAMETER (L_J = L_JSIZE)
      PARAMETER (L_I = L_ISIZE)

C  Number of dust particle sizes

      integer NDP
      parameter (NDP = 1)

C  Number of other tracers (water, clouds. . .)
C  Water is always assumed to be the NDP+1th element, so always set 
C  L_NOT=1

      integer L_NOT
      parameter (L_NOT = 1)

C  Number of tracers in GCM
C  The first NDP tracers are dust, the next L_NOT are the other tracers,
C  water, clouds, . . .
C 
C  This MUST be greater than zero or errors appear in the loops

      integer NTRACE
      PARAMETER (NTRACE = NDP + L_NOT)

