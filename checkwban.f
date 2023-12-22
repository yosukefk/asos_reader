c*******************************************************************************
c     subroutine 15272
c     subroutine to check WBAN section of line to make sure
c     all integers
      subroutine checkwban(awban,iwban1)
      use main1
      implicit none

c iwban1:     integer input WBAN from data file
c i:          loop counter, each iteration of i is
c             digit of WBAN
c j:          ASCII table value of character i of WBAN
c awban:      character string of WBAN

      integer iwban1,i,j
      character awban*5
      
      i=1
      do while(i .le. 5 .and. .not. badwban)
          j=iachar(awban(i:i))
          if (j .lt. 48 .or. j .gt. 57) badwban=.true.
          i=i+1
      enddo
      
      if (.not. badwban) read(awban,'(i5)')iwban1
      return
      end
