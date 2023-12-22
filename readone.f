          
c*************************************************    
c subroutine to read one minute data file
      subroutine readone
      
      use main1
      implicit none

c modified version 14237 to accomodate new flag
c line:     character string of line of data read from one minute data
c03017KDEN DEN2003010100000700   0.091 N                 0.099 N     21      9    25   10   35L60+
c i:        integer file counter
c eof:      integer end-of-file indicator
c totrecs:  integer number of total records read
c ii:       integer counter

      character line*113,flagstr(12)*200,dates(4)*10,adate*8
      integer i,eof,totrecs,ii,ij,idate3(4)
      
c     flag text strings
c     modified for version 14337
      data flagstr /'Non-numeric characters in columns 68 through 90',
     +  'Text string with leading zero in columns 66 through 90',
     +  '4-character numeric string found in columns 30 through 113',
     +  'Column 30 is non-blank',
     +  'No number in columns 70-74 for 2-minute wind direction',
     +  'No number in columns 76-79 for 2-minute wind speed',
     +'No number in columns 82-84 for 5-second gust wind direction',
     +  'No number in columns 87-89 for 5-second gust wind speed',
     +  'Number in column 90',
     +  'Wind speeds and directions outside allowable ranges',
     +  'Other flags switched on, but winds may be okay',
     +  'Other flags switched on, but record is bad'/
c     modified for version 14237
!      data flagstr /'Non-numeric characters in columns 67 through 90',
!     +  'Text string with leading zero in columns 66 through 90',
!     +  '4-character numeric string found in columns 30 through 113',
!     +  'Column 30 is non-blank',
!     +  'Column 67 is non-blank',
!     +  'No number in columns 70-74 for 2-minute wind direction',
!     +  'No number in columns 76-79 for 2-minute wind speed',
!     +'No number in columns 82-84 for 5-second gust wind direction',
!     +  'No number in columns 87-89 for 5-second gust wind speed',
!     +  'Non-blank character in columns 64-68',
!     +  'Number in column 90',
!     +  'Wind speeds and directions outside allowable ranges',
!     +  'Other flags switched on, but winds may be okay',
!     +  'Other flags switched on, but record is bad'/
      
!      data flagstr /'Non-numeric characters in columns 67 through 90',
!     +  'Text string with leading zero in columns 66 through 90',
!     +  '4-character numeric string found in columns 30 through 113',
!     +  'Column 30 is non-blank',
!     +  'Column 67 is non-blank',
!     +  'No number in columns 70 and 71 for 2-minute wind direction',
!     +  'No number in columns 76 and 77 for 2-minute wind speed',
!     +'No number in columns 82 and 83 for 5-second gust wind direction',
!     +  'No number in columns 87 and 88 for 5-second gust wind speed',
!     +  'Wind speeds and directions outside allowable ranges',
!     +  'Other flags switched on, but winds may be okay',
!     +  'Other flags switched on, but record is bad'/
      
!      data flagstr /'Non-numeric characters in columns 67 through 90',
!     +  'Text string with leading zero in columns 66 through 90',
!     +  '4-character numeric string found in columns 30 through 113',
!     +  'Letters D,M, or N not found in column 39, 40, or 41',
!     +  'Number in column 67',
!     +  'No number in columns 70 and 71 for 2-minute wind direction',
!     +  'No number in columns 76 and 77 for 2-minute wind speed',
!     +'No number in columns 82 and 83 for 5-second gust wind direction',
!     +  'No number in columns 87 and 88 for 5-second gust wind speed',
!     +  'Wind speeds and directions outside allowable ranges',
!     +  'Other flags switched on, but winds may be okay',
!     +  'Other flags switched on, but record is bad'/

c     begin reading files
      write(*,14)
      write(ilogunit,14)
      
c open output, error, good record, and summary files     

      open(unit=ierrunit,file=errfil,status='replace')
      open(unit=igdunit,file=goodfil,status='replace')
      open(unit=isusunit,file=suspfil,status='replace')
      
c     put numbers in the columns of errfile and goodfile where wind data should be
      write(ierrunit,19)
      write(igdunit,19)  
      write(isusunit,19)
      
      lnodata=.true.
      baddates(1)=99999999
      baddates(2)=0
      do i=1,nfiles
        eof=0
        open(unit=inunit,file=infiles(i),status='old')
        write(*,15)trim(adjustl(infiles(i)))
        write(ilogunit,15)trim(adjustl(infiles(i)))
        read(inunit,5,iostat=eof)line
10      if (eof .eq. 0) then
c          call the subroutine to read and QA a line of data
          call readline(line)
          if (lwban) goto 99
            
          read(inunit,5,iostat=eof)line
          goto 10
        endif
        close(inunit)
      enddo
      write(*,20)
      write(ilogunit,20)

c if no data has been read within the window set by start and end dates,
c alert user and abort program
      if (lnodata) then
        do ij=1,4
          if (ij .le. 2) then
            idate3(ij)=baddates(ij)
          elseif (ij .eq. 3) then
            idate3(ij)=startdate
          else
            idate3(ij)=enddate
          endif
          write(adate,'(i8)')idate3(ij)
          write(dates(ij),'(2(a2,a1),a4)')adate(5:6),' ',adate(7:8),' ',
     +    adate(1:4)
        enddo
        write(*,24)dates(1),dates(2),dates(3),dates(4)
        write(ilogunit,24)dates(1),dates(2),dates(3),dates(4)
c      else
      endif
c version 09295, summary of record counts
        totrecs=sum(reccounts)
        totrecs=reccounts(1)+reccounts(4)+reccounts(5)+reccounts(6)+
     +  reccounts(7)
        write(ilogunit,21)totrecs,reccounts(1),reccounts(2),
     +   reccounts(3),
     +   reccounts(4)+reccounts(5)+reccounts(6)+reccounts(7),
     +   reccounts(4),reccounts(5),reccounts(6),reccounts(7)
     
c version 09295, summary of flag counts
        write(ilogunit,22)
        do ii=1,12
          if (ii .lt. 12) then 
            write(ilogunit,23)ii,trim(adjustl(flagstr(ii))),
     +      flagcounts(ii)
          else
            write(ilogunit,23)ii-1,trim(adjustl(flagstr(ii))),
     +      flagcounts(ii)
          endif
        enddo
c      endif
      
      write(*,25)
      write(ilogunit,25)

 
      close(ierrunit)
      close(igdunit)  
      close(isusunit)
      
      
      
 5    format(a113)
 14   format(/1x,'######################## 1-MIN FILE PROCESSING ###',
     +    '######################')
 19   format(67x,'123456 1234  123  1234')               
 15   format(/1x,'Reading',1x,a)
 20   format(/1x,'All files read...')
 21   format(/1x,'RECORD FORMAT QA'//1x,
     +  'Total number of records read from files:',t49,i10/1x,
     +  'Number of processed records:',t49,i10/1x,
     +  'Number of records outside data period:',t49,i10/1x,
     +  'Number of records inside data period:',t49,i10/1x,
     +  'Number of non-processed records:',t49,i10/1x,
     +  'Number of records for minute 1:',t49,i10/1x,
     +  'Number of bad records:',t49,i10/1x,
     +  'Number of check records:',t49,i10,/1x,
     +  'Number of bad minute records:',t49,i10/,1x,
     +  'NOTE: Number of non-processed records includes records ',
     +  'outside data period'/)
 22   format(1x,'QA FLAG SUMMARY COUNT'/
     + 1x,'FLAG',t28,'DESCRIPTION',t77,'RECORDS')
 23   format(1x,i2,t8,a,t74,i10)
 24   format(/1x,'NO DATA IN FILES WITHIN DATA PERIOD SET BY USER'//
     + 1x,'DATE RANGE IN FILES: ',a10,' THROUGH ',a10//
     + 1x,'DATE RANGE SET BY USER: ',a10,' THROUGH ',a10//1x
     + 'CHECK DATES IN STARTEND OR DATAFILE LIST'/1x,'STOP PROGRAM')
 25   format(/1x,'###################################################',
     +  '#####################')   
 
 99   return
      end
