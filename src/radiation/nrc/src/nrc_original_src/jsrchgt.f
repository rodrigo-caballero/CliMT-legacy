      function jsrchgt(N,SX,INC,TARGET)

C  March 2002   c-grid
C  GCM2.0  Sept 2002
C
C      Find the first array element that is greater than the 
C      TARGET value.  If N < 1, then 0 is returned.  If no
C      value is found, N+1 is returned.  Replaces Cray version
C      on the workstation.
C      Bisection search implemented 09/29/93 to improve speed.
C
C      Started: 08/23/93
C
C      Input:
C        N      - Number of elements in array SX.
C        SX     - Array of numbers to be searched.  Assumed to be an
C                 ordered array.
C        INC    - Increment between elements of searched array.  Kept
C                 for compatibility with Cray call.
C        TARGET - Value searched for in array SX.
C
C      Output:
C        JSRCHGT - location in array SX where value of SX is first
C                   greater than the TARGET value.
C----------------------------------------------------------------------C

      REAL*8 SX(N), TARGET

C======================================================================C

      if(N.lt.1) then
        ians = 0
      elseif(TARGET.gt.SX(N)) then
        ians = N+1
      elseif(TARGET.lt.SX(1)) then
        ians = 1
      else

        JL = 1
        JH = N

   10   CONTINUE
        if(JH-JL.gt.1) then
          JM = (JL+JH)/2

          if(TARGET.GT.SX(JM)) then
            JL = JM
            JM = (JL+JH)/2
          else
            JH = JM
            JM = (JL+JH)/2
          end if

          GOTO 10
        end if

        if(TARGET.EQ.SX(JH)) JH = JH+1

        ians = JH
      end if

      JSRCHGT = ians

      END
