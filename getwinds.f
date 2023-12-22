c****************************************************
c routine to check for numeric fields in wind data column
      subroutine getwinds(astring,idir1,ispeed1,idir2,ispeed2,iflag)
      use main1
      implicit none

c astring:        character string to read in for 1-minute data
c nums:           character string of numbers
c idir1:          integer wind direction
c speed1:         integer wind speed (knots)
c idir2:          integer wind gust direction
c ispeed:         integer wind gust (knots)
c iflag:          flag indicating QA status of data line
c ifield:         integer number of fields read in
c i:              integer counter
c cnum:           real number being read from astring
c lstart:         logical variable denoting numeric string starting

      character astring*(*),nums*10
      integer idir1,ispeed1,idir2,ispeed2,iflag,ifield,i
      real cnum
      logical lstart
      cnum=0.0
      ifield=0
      
      nums='0123456789'
      lstart=.false.
c check string for 4 fields
c     version 14337, reset loop to 23 to account for astring being columns 68:90 of text record
!      do i=1,24
       do i=1,23
        if (astring(i:i) .ne. ' ') then
          if (astring(i:i) .ge. '0' .and. astring(i:i) .le. '9') then
            lstart=.true.
            CNUM = CNUM*10.+float(index(nums,astring(i:i))-1)
            if (i .eq. 23) ifield=ifield+1
          endif 
        else
          if (lstart) then
            ifield=ifield+1
            lstart=.false.
            cnum=0.0
          endif   
        endif     
      enddo
   
      if (ifield .eq. 4 .or. ifield .eq. 5) then
        read(astring,*)idir1,ispeed1,idir2,ispeed2
        if ((idir1 .gt. 360 .or. idir1 .lt. 0) .or. 
     +  (idir2 .gt. 360 .or. idir2 .lt. 0) .or. 
     +  (ispeed1 .ge. 50 .or. ispeed1 .lt. 0) .or.
     +  (ispeed2 .ge. 50 .or. ispeed2 .lt. 0 )) then
         iflag=8   
        else
          iflag=ifield
        endif
      else
        iflag=ifield
      endif
     
      return
      end
