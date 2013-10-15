      subroutine whenflt(n,array,inc,target,index,nval)
c
c $Id: whenflt.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
	dimension index(*), array(*)
	ina=1
	nval=0
	if(inc .lt. 0) ina=(-inc)*(n-1)+1
	do 100 i=1,n
	    if(array(ina) .lt. target) then
	    nval=nval+1
	    index(nval)=i
	    end if
	    ina=ina+inc
 100    continue

      end           
