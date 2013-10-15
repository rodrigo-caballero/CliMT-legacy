C
C Offsets for both advected and non-advected tracer species
C
        integer ixuadv	     ! user advected tracer beginning index
	integer ixunad	     ! user non-advected tracer beginning index
	integer ixcldw	     ! cloud water beginning index
	integer ixtrcg       ! greenhouse gas beginning index
	integer ixtrct       ! test tracers beginning index

	common/ trcindx / ixuadv, ixunad, ixcldw, ixtrcg, ixtrct
