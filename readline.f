c***************************************************************
c subroutine to process a line
      subroutine readline(line)
      use main1
      implicit none

c iflag:    integer indicator of validity of record (0=valid,1=invalid) 
c imin:     integer minute of data being read from record
c iyear:    4-digit integer year of data record
c imonth:   2-digit integer month of data record
c iday:     2-digit integer day of month of data record
c ihr:      2-digit integer hour of day (00-23)
c idate1:   integer date of record (YYYYMMDDHHMM)  
c idate2:   character string (length 12) of original date of record (YYYYMMDD)      
c iwban1:   integer WBAN number of station read from data record
c           this number replaces the WBAN entered by user 
c           and program issues warning that WBAN has changed
c idir:     integer 3-digit 2-minute average wind direction read from record
c ispeed:   integer average 2-minute wind speed (knots) read from record
c n:        integer month counter relative to starting month of data period
c           for firsth month of data n=1, 2nd month=2, etc.
c rmin:     real equivalent of imin
c frac:     real result of MOD equation used to determine if minute is even or odd
c line:     record being read and processed (113 length character string)
c aflag:    11-character string of flags from checkwind
c stat1:    4-character string of station call sign
      integer imin,idir,iflag,iyear,imonth,iday,ihr,idate1,iwban1,
     + ispeed,n
      real rmin,frac
      
      character line*113,idate2*12,aflag*11,stat1*4
     
c     read minute of record, convert to real, and get frac      
      read(line(24:25),'(i2)')imin
      rmin=real(imin)     
      frac=mod(rmin,2.0)
      
c do not process minute 1 as it straddles 2-hours      
      if (frac .ge. 0.0 .and. imin .ne. 1) then
c       perform QA on segment of record that contains wind speed and direction
        call checkwind(line,iflag,aflag,idir,ispeed)
c       if iflag = 0, record appears valid, continue processing
        if (iflag .eq. 0) then
          irec=irec+1
           reccounts(1)=irec
c         write line to file of valid records for later check     
          write(igdunit,7)line  
c         read WBAN # and check against user entered WBAN
c         change to iwban1 and warn user at end of program
c         version 15272, check to make sure WBAN section is all integers
          iwban1=0
          call checkwban(line(1:5),iwban1)
          if (badwban) then
              write(*,9)
              write(ilogunit,9)
              goto 5
          endif
c          read(line(1:5),'(i5)')iwban1 
          stat1=line(6:9)
          if (irec .eq. 1) then 
            iwban=iwban1
            iwban2=iwban1
            stat=stat1
            stat2=stat1
          endif
c         if WBAN changes, notify user and stop program
          if (iwban1 .ne. iwban) then
            lwban=.true.
              write(*,6)iwban,iwban1
              write(ilogunit,6)iwban,iwban1
              goto 5
          endif
          if (stat1 .ne. stat) then
            stat=stat1
            lstat=.true.
            statcnt=statcnt+1
            if (statcnt .le. 10) stats(statcnt)=stat1
          endif
c         get year, month, day, and hour of record
          read(line(14:23),'(i4,3i2)')iyear,imonth,iday,ihr
c          get date before adding 1 hour
          write(idate2,'(i4,4i2.2)')iyear,imonth,iday,ihr,imin
           
c         hourly averages will be considered hour ending. so add 1 to hour
c         and change date accordingly (hour 0 is hour 24 of day before)
          call changehr(iyear,imonth,iday,ihr,imin)
c         calculate date variable 
c         example January 1, 2001 would be 20010101
c         check against the start and end dates
c         and if outside the range exit subroutine
          idate1=(iyear*10000)+(imonth*100)+iday
          if (startdate .gt. idate1 .or. idate1 .gt. enddate) then
            baddates(1)=min(baddates(1),idate1)
            baddates(2)=max(baddates(2),idate1)  
            reccounts(2)=reccounts(2)+1
            goto 5
          else
            reccounts(3)=reccounts(3)+1
          endif
          lnodata=.false.
c         calculate month relative to start month
c         if current month is March 2002 and start month is January 2001
c         then n = 15
          n=(iyear-startyear)*12+imonth-(startmon)+1
c         assign idate1 to date array
c         example:  for March 7, 2002 with start date of January 2001 n=15 iday=7
          idate(n,iday)=idate1
          origdate(n,iday,ihr,imin)=idate2
      
c          check speeds
c          if wind speed is less than 2 knots
c          check to see if ASOS is not part of the IFW group or if the date
c          of the current record is before the IFW group date date
c          if wind speed is less than 2 knots and station is not part of IFW at all 
c          or at time of record, set speed to 1 knot (0.51 m/s), set the keep flag for the 
c          minute to 0 the calm flag for the minute to 1.
c          if wind speed is less than 2 knots and the station is part of IFW at the time of the record
c          then set the keep flag for the minute to 1 and convert the speed to m/s. 
c          if the wind speed is 2 knots or greater, set the keep flag to 1 and convert speed
c          to m/s
          if (ispeed .lt. 2) then
            if (.not. ifw .or. ifwdate .gt. idate1) then  
              ikeep(n,iday,ihr,imin)=0                  !this minute will not be used in wind direction averages
              icalm(n,iday,ihr,imin)=1
              speed1(n,iday,ihr,imin)=0.51              !set wind speed to 1 knot (0.51 m/s)
            else
              ikeep(n,iday,ihr,imin)=1  
              icalm(n,iday,ihr,imin)=0         
              speed1(n,iday,ihr,imin)=(real(ispeed)+trunc)*0.51    
            endif
          else
            speed1(n,iday,ihr,imin)=(real(ispeed)+trunc)*0.51
            ikeep(n,iday,ihr,imin)=1
            icalm(n,iday,ihr,imin)=0
          endif
          speedkts(n,iday,ihr,imin)=real(ispeed)
c calculate x and y components of wind direction
c regardless of the value of the keep flag or calm flag
          dirminut(n,iday,ihr,imin)=real(idir)
          xdir(n,iday,ihr,imin)=-1.0*sin(real(idir)*d2r)   !get x and y components of direction
          ydir(n,iday,ihr,imin)=-1.0*cos(real(idir)*d2r)
          idirmin(n,iday,ihr,imin)=idir
      else
c if a bad record (iflag > 0), write to bad records file for later checking
          if (aflag(11:11) .eq. '4' .or. aflag(11:11) .eq. '5') then
            write(isusunit,8)line,aflag
            reccounts(6)=reccounts(6)+1
          else
            write(ierrunit,8)line,aflag
            reccounts(5)=reccounts(5)+1
          endif
        endif    
      else
        if (imin .eq. 1) then
            reccounts(4)=reccounts(4)+1
        else
            reccounts(7)=reccounts(7)+1
        endif    
      endif

 6    format(/1x,'WBAN number has changed from ',i5,1x,'to',1x,i5/1x,
     +  'Stopping AERMINUTE')
 7    format(a113)
 8    format(a113,1x,a13)
 9    format(/1x,'Bad WBAN string in file, stopping AERMINUTE')
 5    return
      end
