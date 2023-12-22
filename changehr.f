c*******************************************************************************
c subroutine changehr
c called from readline
c this subroutine adds 1 to the hour of the record from the one minute file
c also changes hour 0 and minute 0 to hour 24 minute 60 of previous day
      subroutine changehr(iyear,imonth,iday,ihr,imin)
      use main1
      implicit none

c iyear:    integer 4-digit year of record 
c imonth:   integer 2-digit month of record
c iday:     integer 2-digit day of record
c ihr:      integer 2-digit hour of record
c imin:     integer minute of record 
c days:     integer number of days per month, set February = 28
c leap:     logical variable denoting if year is leap year (true=leap year, false=non-leap year)


      integer imin,ihr,iday,imonth,iyear
      logical leap

c if hour is 0 and minute is 0, change hour to 24 and change day to day before date of record
c example:  hour 0 minute 0 for March 2, 2002:  date becomes March 1, 2002, hour 24, minute 60

      if (ihr .eq. 0 .and. imin .eq. 0) then
        ihr=24
        imin=60
c        if first day of month set to the last day of previous month
c        and reset month to previous month as well
c        if January 1, reset year to previous year, meaning minute will not be used at all if 
c        start date of processing is January 1
        if (iday .eq. 1) then
          if (imonth .eq. 1) then
            iday=days(12)
            imonth=12
            iyear=iyear-1
          else
c           if March 1, check to see if year is leap year and reset to Feb. 28 or 29 accordingly
            if (imonth .eq. 3) then
              call leapyr(iyear,leap)
              if (leap) then
                iday=29
              else
                iday=28
              endif
            else
              iday=days(imonth-1)
            endif
            imonth=imonth-1
        endif
       else
           iday=iday-1
         endif
      else
c       if imin is not equal to zero, just add 1 to hour (if minute = 2 and hour =23, hour =24)
        if (imin .gt. 0) then
          ihr=ihr+1
c       if hour is not zero and minute is 60, just reset minute to 60
        else
          imin=60
        endif
      endif
      
      return
      end
