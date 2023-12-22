     
c***************************************************************
c subroutine leapyr
c called from changehr, makedate
c determines if year is leap year
c calculations based on formulas found in Frequently Asked Questions about Calendars," Version 2.2,
c by Claus Tondering, April 9, 2000, available on the web at URL
c http://www.tondering.dk/claus/calendar.html
c from website:
c The Gregorian calendar has 97 leap years every 400 years: 

c Every year divisible by 4 is a leap year. 
c However, every year divisible by 100 is not a leap year. 
c However, every year divisible by 400 is a leap year after all. 
c So, 1700, 1800, 1900, 2100, and 2200 are not leap years. But 1600, 2000, and 2400 are leap years. 


      subroutine leapyr(iyear,leap)
      use main1
      implicit none
      
c iyear:    integer 4-digit year of processed year
c ileap4:   integer result of MOD(iyear,4)
c ileap100: integer result of MOD(iyear,100)
c ileap400: integer result of MOD(iyear,400)
      integer iyear,ileap4,ileap100,ileap400
      logical leap
      
      ileap4=mod(iyear,4)   
      ileap100=mod(iyear,100)
      ileap400=mod(iyear,400)

c if year is not divisible by 4 or year is divisible by 100 but not divisible by 400 then year 
c is not a leap year
c 2001 is not a leap year because it is not divisible by 4
c 2000 is is a leap year because it is divisible by 400      
      if (ileap4 .ne. 0 .or. 
     +  (ileap100 .eq. 0 .and. ileap400 .ne. 0)) then 
        leap=.false.  
      else
        leap=.true.  
      endif
      return
      end
