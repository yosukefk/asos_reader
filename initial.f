      
c*************************************************************             
c subroutine to initialize arrays
      subroutine initial
      use main1
      implicit none

c i:        integer month counter
c imon:     integer month of year
c iyear:    integer 4 digit year
c leap:     logical variable denoting if year is leap year

      integer i,imon,iyear
      logical leap
      
      write(*,1)

      
  1   format(1x,'Initializing arrays...'/)
  
      leap=.false.
c get number of months to be processed   
      nmonths=(12-startmon+1)+((endyear-startyear-1)*12)+endmon
   
c allocate arrays   
      allocate(ndays(nmonths)) 
      allocate(idate(nmonths,31))
      allocate(origdate(nmonths,31,24,60))
      allocate(ikeephr(nmonths,31,24))
      allocate(ikeep(nmonths,31,24,60))
      allocate(icalm(nmonths,31,24,60))
      allocate(idirmin(nmonths,31,24,60))
      allocate(nvalid(nmonths,31,24))
      allocate(ncalms(nmonths,31,24))
      allocate(neven(nmonths,31,24))
      allocate(nevencalm(nmonths,31,24))
      allocate(nodd(nmonths,31,24))
      allocate(noddcalm(nmonths,31,24))
      allocate(noddperm(nmonths,31,24))
      allocate(noddpermc(nmonths,31,24))
      allocate(speed(nmonths,31,24))
      allocate(speed1(nmonths,31,24,60))
      allocate(speedkts(nmonths,31,24,60))
      allocate(xdir(nmonths,31,24,60))
      allocate(ydir(nmonths,31,24,60))
      allocate(dirminut(nmonths,31,24,60))
      allocate(xdirhr(nmonths,31,24))
      allocate(ydirhr(nmonths,31,24))
      allocate(dirhr(nmonths,31,24))
      allocate(speedmin(nmonths,31,24))
      allocate(speedmax(nmonths,31,24))
      allocate(dirmin(nmonths,31,24))
      allocate(dirmax(nmonths,31,24))
      allocate(srcmin(2,nmonths,31,24))
      allocate(srcmax(2,nmonths,31,24))
!      allocate(nhours(nmonths,5))
!      allocate(nhours(nmonths,4))
c     allocate arrays for comparisons between standard 
c     obs and one minute data
      if (comp) then
        allocate(obspd(nmonths,31,24,60))
        allocate(obdir(nmonths,31,24,60))
        allocate(flag(nmonths,31,24,60))
        allocate(qcflag(nmonths,31,24,60))
        obspd=-9.0
        obdir=-9.0
      endif   
c 15272
      allocate(minsrc(nmonths,31,24,60))
      if (lfive) then
          allocate(nvalid5(nmonths,31,24))
          allocate(neven5(nmonths,31,24))
          allocate(nodd5(nmonths,31,24))
          allocate(ikeep5(nmonths,31,24,12))
          allocate(icalm5(nmonths,31,24,12))
          allocate(idir5min(nmonths,31,24,12))
          allocate(speed5(nmonths,31,24,12))
          allocate(xdir5(nmonths,31,24,12))
          allocate(ydir5(nmonths,31,24,12))
          allocate(dir5minut(nmonths,31,24,12))
          allocate(nhours(nmonths,6))
          allocate(speed5kts(nmonths,31,24,12))
          allocate(origdate5(nmonths,31,24,12))
          ikeep5=-9
          icalm5=0
          idir5min=0
          nvalid5=0
          xdir5=-999.0
          ydir5=-999.0
          dir5minut=-999.0
          nvalid5=0
          neven5=0
          nodd5=0
          speed5kts=-999.0
      else
          allocate(nhours(nmonths,5))
      endif
c     now initialize the ndays array
c     and calculate # of hours in data period
      itothrs=0
      do i=1,nmonths
        if (i .eq. 1) then
          imon=startmon
          iyear=startyear
        else if (i .eq. nmonths) then
          imon=endmon
        else
          if (imon .eq. 12) then
            imon=1
            iyear=iyear+1
          else
            imon=imon+1
          endif
        endif     
        if (imon .eq. 2) then
          call leapyr(iyear,leap)
          if (leap) then 
c             ndays(i)=29
            ndays(i)=days(imon)+1
          else 
c             ndays(i)=28
            ndays(i)=days(imon)
          endif
        else
         ndays(i)=days(imon)
c           if (imon .eq. 4 .or. imon .eq. 6 .or. imon .eq. 9 .or. 
c     +      imon .eq. 11) then
c             ndays(i)=30
c           else
c             ndays(i)=31
c           endif
        endif
        itothrs=itothrs+(ndays(i)*24)
      end do
      
            
c set initial values 
      ikeephr=-9
      idate=0
      speed=0.0
      xdirhr=0.0
      ydirhr=0.0
      speed1=-999.0
      speedkts=-999.0
      dirminut=-999.0
      xdir=-999.0
      ydir=-999.0
      ikeep=-9
      dirhr=-999.0
      speedmin=0.0
      speedmax=0.0
      dirmin=0
      dirmax=0
      nvalid=0
      idirmin=-9
      ncalms=0
      neven=0
      nevencalm=0
      nodd=0
      noddcalm=0
      noddpermc=0
      noddperm=0
      srcmin=-9
      srcmax=-9
      minsrc=1
      lwban=.false.
      badwban=.false.
      lstat=.false.
      irec=0
      flagcounts=0
      reccounts=0
      nhours=0
      wbancnt=0
      statcnt=0
      return
      end
