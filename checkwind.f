c**************************************************************
c called from readline
c subroutine to check for non-numeric characters in columns for direction and speed
c input is record of one-minute file and outputs are a flag, wind direction and speed
c version 14237, modified checks for flags 6-9 to account for change in format of 1-minute
c data files
      
      subroutine checkwind(var1,iflag,aflag,idir1,ispeed1)  
      use main1
      implicit none
c updated version 14337      
c var1:     line of one minute data record being processed
c junk:     1-character variable with value 0 through 9
c zeros:    3-character array to check for leading zeros
c aflag:    11 character string of iflag1 values
c iflag:    integer variable denoting if record is to be further processed (0=yes,1=no)
c           uses values from array iflag1
c iflag1:   10 element integer array of flags for 9 error checks
c           values
c           0=valid
c           1=invalid or suspicious
c           positions
c           1=non-numeric characters seen in columns 68 through 90
c           2=leading zero
c           3=4-digit string that may be a time
c           4=non-blank value in column 30
c           5=wind direction problem 
c           6=wind speed problem 
c           7=gust direction problem 
c           8=gust speed problem 
c           9=column 90
c           10=bad wind values
c           12=read for numeric fields in wind locations without checking for specific columns but
c           data does not fit column specifications, i.e. iflag1(6)-iflag1(9) = 1
c iflag2    number of fields read by getwinds when data passes strict QA checks
c idir1:    integer 2-minute average wind direction
c ispeed1:  integer 2-minute average wind speed (knots)
c idir2:    integer 5-second peak wind speed's wind direction
c ispeed2:  integer 5-second peak wind speed (knots)
c j1,j2:    integer values for index function
c i2:       integer counter for chars and chars2
c i1:       integer counter
c p1:       character code for leading blank of 4-character string for flag 3
c p2:       character code for first character of 4-character string for flag 3
c p3:       character code for second character of 4-character string for flag 3
c p4:       character code for third character of 4-character string for flag 3
c iv1:      4-element array of start columns to check for wind data
c iv2:      4-element array of end columns to check for wind data
c c:        column loop counter
c c1:       column counter equal to values between iv1 and iv2
c c2:       column loop counter
c v:        array of character codes
c id:       loop counter equal to iv2-iv1+1
c aflag:    11 character string of iflag1 values
c noblanks: string being read has noblanks
      


      character var1*113,junk*1,zeros(10)*3,aflag*11,c39*1,c40*1,c41*1
      character str*84,fields(42)*84
      logical l1,notnum,noblanks
      integer iflag,iflag1(11),idir1,ispeed1,j1,j2,j3,ilen1,idir2,
     +  ispeed2,i1,i2,p1,p2,p3,p4,iv1(4),iv2(4),c,c1,c2,v1,v2,ifield,k,
     +  i,ii,id,iflag2
      integer, allocatable, dimension(:) :: v
      character form*35
c set values for zeros
      data zeros /' 00',' 01',' 02',' 03',' 04',' 05',' 06',' 07',' 08',
     +  ' 09'/
    
      data iv1 /70,76,82,87/
      data iv2 /74,79,84,89/
       
c initialize wind directions and speeds as well as iflag and iflag1 
      idir1=-9
      ispeed1=-9
      idir2=-9
      ispeed2=-9
      iflag=0
      iflag1=0
      fields=''
c example of record
c                                                                   123456 1234  123  1234
c13722KRDU RDU2001010101470647   0.087 N     0.086 N                264      5   258    5 
c in first line above 123456 refers to columns 68-73, the first 1234 are columns 75-78,
c 123 are columns 81-83 and the last 1234 are columns 86-89

c check record for a time string, 0100, 2314 outside of the station/date information block
c sometimes records appear to be "jammed" into another record, correcting for time
c if such a record is found, do not use.
c      i2=1
c      do while (iflag1(1) .eq. 0 .and. i2 .le. 1440)
c        j1=index(var1(30:90),times(i2))
c        if (j1 .gt. 0) then
c          iflag1(1)=1
c        else
c          i2=i2+1
c        endif  
c      enddo
           


c check columns 67 through 90 for any nonnumeric characters
c if any found do not use line
c     version 14337, reset i2 to 68      
!      i2=67
      i2=68
      do while (iflag1(1) .eq. 0 .and. i2 .le. 90)
        j1=iachar(var1(i2:i2))
        if (j1 .eq. 32 .or. (j1 .ge. 48 .and. j1 .le. 57)) then
          i2=i2+1
        else
          iflag1(1)=1
        endif
      enddo
      

      
c there should be not be anything with a leading zero, i.e 01, 02, 035, etc. 
      i2=1
      do while (i2 .le. 10 .and. iflag1(2) .eq. 0)
        j1=index(var1(30:90),zeros(i2))
        if (j1 .gt. 0) then
          iflag1(2)=1
        else
          i2=i2+1
        endif
      end do


               

c    version 11298 check for numeric fields in columns 30-113 to see if possible leading time
c     if appears to be a time or a set of numeric characters with double decimals, flag as
c     suspicious
      i=30
      ifield=1
      l1=.false.
      k=0
      do while(ifield .le. 42 .and. i .le. 113)
        i1=iachar(var1(i:i))
        if (i1 .ne. 32) then
          l1=.true.
          k=k+1
          str(k:k)=var1(i:i)
        else
          if (l1) then
            fields(ifield)=str
            ifield=ifield+1
            l1=.false.
            k=0
            do ii=1,84
              str(ii:ii)=' '
            enddo
          endif
        endif
       i=i+1
      enddo

c check for numerical fields and determine if a possible 4-digit time
      do i=1,ifield
        str=fields(i)
        call checkfield(str,iflag1(3))
      enddo


c     check column 30, should always be blank

      j1=iachar(var1(30:30))
      if (j1 .ne. 32) iflag1(4)=1
      

         
c now check column 67.  there should not be a number there 
c version 298:  should be blank, any other than blank, flag
c version 14337, do not check
!      j1=iachar(var1(67:67))
!      if (j1 .ne. 32) iflag1(5)=1
         
!      if (j1 .ge. 48 .and. j1. le. 57) iflag1(5)=1  
           
c check column 71 and 70.  Column 70 should have a number if the wind direction is in 68-70, 69-71, or 70-72
c column 71 should have a number for wind direction in columns 69-71, 70-72, or 71-73

c check columns 70 and 71 for 2-minute wind direction, 76 and 77 for 2-minute wind speed,
c 82 and 83 for 5-second gust, and 87 and 88 for 5-sec gust speed
c experience has shown that data should be in these columns
c if no number is found, flag the record


c version 14237, modify checks for directions and speeds based on new format of 1-minute files
c data appears to be shifted to the right in late 2013 and later files
      do c=1,4
          noblanks=.false.
          id=iv2(c)-iv1(c)+1
          allocate(v(id))
          c1=iv1(c)
          do c2=1,id
              v(c2)=iachar(var1(c1:c1))
              if (v(c2) .ne. 32)noblanks=.true.
              c1=c1+1
          enddo 
          
          do c2=1,id
              if ((v(c2) .lt. 48 .and. v(c2) .ne. 32) 
     +        .or. v(c2) .gt. 57) iflag1(c+4)=1
              if (c2 .gt. 1 .and. c2 .lt. id) then  !blank between 2 non-blanks
                  if (v(c2) .eq. 32. .and. v(c2-1) .ne. 32 .and. 
     +            v(c2+1) .ne. 32) iflag1(c+4)=1
              endif
!              if (.not. noblanks) iflag1(c+5)=1  !all blanks
              if (.not. noblanks) iflag1(c+4)=1  !all blanks
          enddo
          deallocate(v)
      enddo


c     version 14337, don't check
c     version 14237, new flag,flag 10, no numbers in columns 64-68
!      do i=64,68
!          j1=iachar(var1(i-1:i-1))
!          j2=iachar(var1(i:i))
!          j3=iachar(var1(i+1:i+1))
!          if (j1 .eq. 32 .and. j2 .ne. 32 .and. j3 .eq. 32) iflag1(10)=1
!      enddo


c     version 14237, check column 90, if something there make iflag1(9) = 1
      allocate(v(1))
      v(1)=iachar(var1(90:90))
      if (v(1) .ne. 32) iflag1(9)=1
      deallocate(v)

     

c read 2-minute average wind direction and speed and 2-minute gust and speed
c version 301, change wind speed to catch for 50 knots
c version 14337, read 68:90
      if (maxval(iflag1) .eq. 0) then            !possible good record according strict checks
        call getwinds(var1(68:90),idir1,ispeed1,idir2,ispeed2,
     +    iflag2)
        
        if (iflag2 .eq. 4) then
          read(var1(68:90),*)idir1,ispeed1,idir2,ispeed2
          if ((idir1 .gt. 360 .or. idir1 .lt. 0) .or. 
     +    (idir2 .gt. 360 .or. idir2 .lt. 0) .or. 
     +    (ispeed1 .ge. 50 .or. ispeed1 .lt. 0) .or.
     +    (ispeed2 .ge. 50 .or. ispeed2 .lt. 0 )) then
             iflag1(10)=1
c              iflag1(12)=1
c             goto 100
          endif
         iflag1(11)=9
c          iflag1(13)=9
        else
c          iflag1(13)=iflag2
c          iflag1(12)=1 
          iflag1(11)=iflag2
          iflag1(10)=1 
        endif
      else
c      record does not have non-numeric characters in columns 68-90
c      and there are no leading zeros and and no n4-digit numbers
c      and wind data may not match specific column criteria
c      but other flags maybe switched on
c     version 14337, read columns 68:90
c        if (iflag1(2) .eq. 0 .and. iflag1(4) .eq. 0 .and. 
c     +   (iflag1(5) .ge. 0 .or. iflag1(6) .ge. 0 
c     +   .or. iflag1(7) .ge. 0 .or. iflag1(8) .ge. 0)) then    
        if (iflag1(1) .eq. 0 .and. iflag1(2) .eq. 0) then  
!         call getwinds(var1(67:90),idir1,ispeed1,idir2,ispeed2,
         call getwinds(var1(68:90),idir1,ispeed1,idir2,ispeed2,
c     +    iflag1(13))
     +    iflag1(11))
        endif
      endif

c get maximum flag value for first 10 flags
c also if flag1(i2) is not zero add to flagcounts
c version 14132, now first 11 flags
      do i2=1,10
c      do i2=1,12
        write(aflag(i2:i2),'(i1)')iflag1(i2)
        iflag=max(iflag,iflag1(i2))
        if (iflag1(i2) .ne. 0) flagcounts(i2)=flagcounts(i2)+1
      enddo
      
      write(aflag(11:11),'(i1)')iflag1(11)
c      write(aflag(13:13),'(i1)')iflag1(13)

c get counts for last flag, flag 12
c if flag 11 = 4 then add 1 to flagcounts(11), count of records that are suspicious
c otherwise add 1 to flagcounts(12), count of records that are bad

      if (iflag .ne. 0) then
        if (iflag1(11) .eq. 4 .or. iflag1(11) .eq. 5) then 
            flagcounts(11)=flagcounts(11)+1   !count for check records
        else
            flagcounts(12)=flagcounts(12)+1   !count for bad records
        endif
      endif
       
 100  return
      end
