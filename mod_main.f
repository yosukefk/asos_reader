c**********************************************************************************
      module main1
      implicit none

c DATE VARIABLES
c startmon:     Integer month of start of processing period
c startday:     Integer day of start of processing period
c startyear:    Integer 4-digit year of start of processing period
c endmon:       Integer month of end of processing period
c endday:       Integer day of end of processing period
c endyear:      Integer 4-digit year of end of processing period
c startdate:    Integer start date (YYYYMMDD)
c enddate:      Integer end date (YYYYMMDD) 
c ifwdate:      Integer date (YYYYMMDD) of date of IFW commission date
c               If no IFW commission date then ifwdate set to 99999999
c days:         Integer array of number of days per calendar month
c               February is listed at 28 days
c baddates:     2 dimensional array of minimum and maximum dates read
c               that are outside window set by start and end dates
c               the values for these dates will only be displayed
c               if the entire time period from the one minute files
c               is completely outside the window set by the start and end dates
c baddate5:     2 dimensional array of minimum and maximum dates read
c               that are outside window set by start and end dates
c               the values for these dates will only be displayed
c               if the entire time period from the 5 minute files
c               is completely outside the window set by the start and end dates
      integer startmon,startday,startyear,endmon,endday,endyear,
     +  startdate,enddate,ifwdate,days(12),baddates(2),baddate5(2)

c other integer variables
c iwban:        Integer WBAN number of station.  This number can be found in the one minute data file
c iwban2:       Integer WBAN number of station from first record of first file.  Retained through program
c               even if WBAN changes based on one-minute data files
c wbancnt:      Integer number of times WBAN changes while reading data (10 is maximum)
c statcnt:      Integer number of times station ID changes while reading data (10 is maximum)
c nmonths:      Integer number of months in processing period
c nfiles:       Integer number of one-minute data files to be read in
c nfiles5:       Integer number of five-minute data files to be read in
c nmin:         Integer month (relative to start month) of minimum one-minute wind
c daymin:       Integer day of month of minimum one-minute wind
c hrmin:        Integer hour of day of minimum one-minute wind
c               this value will be subtracted by 1 hour when displayed
c               to show the actual time the minimum speed occurred
c minmin:       Integer minute of hour of minimum one-minute wind
c minsrc1:      Integer to indicate source of minimum wind speed (1=1-minute 5=5-minute data)
c nmax:         Integer month (relative to start month) of maximum one-minute wind
c daymax:       Integer day of month of maximum one-minute wind
c hrmax:        Integer hour of day of maximum one-minute wind
c               this value will be subtracted by 1 hour when displayed
c               to show the actual time the maximum speed occurred
c minmax:       Integer minute of hour of maximum one-minute wind
c maxsrc1:      Integer to indicate source of maximum wind speed (1=1-minute 5=5-minute data)
c nhrmin:       Integer month (relative to startmonth) of minimum hourly averaged wind speed
c dayhrmin:     Integer day (relative to startmonth) of minimum hourly averaged wind speed
c hrhrmin:      Integer hour of day of minimum hourly averaged wind speed
c nhrmax:       Integer month (relative to startmonth) of maximum hourly averaged wind speed
c dayhrmax:     Integer day (relative to startmonth) of maximum hourly averaged wind speed
c hrhrmax:      Integer hour of day of maximum hourly averaged wind speed
c itothrs:      Integer number of total calendar hours in data period
c               regardless if all hours in one-minute data
c               i.e. if processing a year, itothrs=8760 even if there 
c               are only 7000 hours of one minute data
c irec:         Integer record counter used for station and WBAN comparison
c flagcounts:   Integer array of flag counts for each QA flag in checkwinds
c               subscripts 1-11 refer to first 9 flags in checkwind
c               subscript 12 is for suspicious records
c               subscript 13 is for bad records
c               added in version 09295  
c               adjusted in version 14237 and 14337
c reccounts     integer array of record counts (added version 09295)
c               subscript 1 = good records (processed records)
c               subscript 2 = records outside data period
c               subscript 3 = records inside data period
c               subscript 4 = records for minute 1 of the hour
c               subscript 5 = bad records (fail QA check completely)
c               subscript 6 = suspicious records
c               subscript 7 = records without a minute (should always be zero)
c wbans         Array of WBAN numbers that change while reading data
c nsurf:        Integer number of surface ISHD files to read to compare
c               one minute data

      integer iwban,iwban2,wbancnt,statcnt,nmonths,nfiles,nmin,daymin,
     +  hrmin,minmin,nmax,daymax,hrmax,minmax,nhrmin,dayhrmin,hrhrmin,
     +  nhrmax,dayhrmax,hrhrmax,itothrs,irec,flagcounts(12),
     +  reccounts(7),wbans(10),nsurf,nfiles5,minsrc1,maxsrc1

c file units
c mainunit:     Integer input file unite of main input control file, aerminute.inp (24)
c inunit:       Integer input file unit (12) of one-minute data file(s)
c inunit5:      Integer input file unit (24) of 5-minute data file(s)
c ioutunit:     Integer output file unit (16) of hourly averaged winds
c ierrunit:     Integer unit (17) of error file.  This file contains the 
c               bad records read from the one minute data.
c ilogunit:     Integer unit (20) of log file of one minute processing
c               These are records that do not fit the format of the files
c isumunit:     Integer unit (21) of summary file. This file contains the
c               number of minutes read, total calms, even minutes, even calms,
c               odd minutes read, odd calm minutes read, odd minutes used, and
c               odd calm minutes used for each hour
c igdunit:      Integer unit (22) of good records file.  This file contains the records
c               that are read into the wind variables' arrays.  
c isusunit:     Integer unit (23) of suspicious records files.  This file contains the
c               records that are possible viable records but should be checked
c ierrunit5:    Integer unit (25) of error file for 5-minute data.  This file contains the 
c               bad records read from the 5- minute data.
c igdunit5:     Integer unit (26) of good records file for 5-minute data. This file contains the records
c               that are potentially substituted into the wind variables' arrays. 
c iclmvar5:     Integer unit (27) of 5-minute calm and variable winds.
c icomp5:       Integer unit (28) comparing 1-minute and 5-minute winds for same minute
c isubumit:     Integer unit (29) listing substituted minutes
      integer mainunit,inunit,ioutunit,ierrunit,ilogunit,isumunit,
     + igdunit,isusunit,inunit5,igdunit5,ierrunit5,iclmvar5,icomp5,
     + isubunit

c real variables
c dtr:          Conversion factor for degrees to radians (pi/180 degrees)
c ht:           Aneomemeter height input by user.
c trunc:        Factor to account for truncation of wind speeds by ASOS processing
c               Currently, 1/2 knot added to account that winds are truncated by
c               ASOS when calculating the 2-minute average of 5 second speeds
c               Version 09239, dated 8/27/09 truncation factor set to 0
c               Truncation account will take place in AERMET
c minonemin:    Minimum one minute wind speed
c maxonemin:    Maximum one minute wind speed

      real d2r,ht,trunc,minonemin,maxonemin
 
c character    
c versn:        6 character string of one minute program version number
c stat:         4 character string of station ID read from files
c stat2:        4 character string of station ID read from first record of first file
c amonths:      Array of month names
c inpfile:      Name of text file containing the names of the data files
c infiles:      Character name of array of input one minute files
c infiles5:      Character name of array of input five minute files
c outfil:       Name of output hourly averaged winds file
c errfil:       Name of bad records file (bad_records.dat)
c logfil:       Name of log file (areminute.log)
c sumfil:       Name of summary file (minute_counts.dat)
c goodfil:      Name of good records file (good_records.dat)
c suspfil:      Name of file with suspicious records (check_records.dat)
c origdate:     4 dimensional array of dates (YYYYMMDD) dimension nmonths x 31 days x 24 hours x 60 mins
c origdate5:    4 dimensional array of dates (YYYYMMDD) for 5-minute data dimension 
c               nmonths x 31 days x 24 hours x 12 mins
c stats:        Array of character string of station IDs if station IDs change while reading data
c               can only be upto 10 changes
c compfil:      Name of file comparing one minute data against standard observations
c obfiles:      Array of filenames of ISHD standard observation data
c flag:         4 dimensional array flag denoting if standard observation is 
c               calm, valid, variable, or missing
c               dimension nmonths x 31 days x 24 hours x 60 mins
c qflag:         4 dimensional array flag denoting QC code of standard observation
c               dimension nmonths x 31 days x 24 hours x 60 mins 
c goodfil5:     Name of good records file for 5-minute data (good_records_5.dat)
c errfil5:       Name of bad records file for 5-minute data (bad_records_5.dat)
c clmvar5:      Name of calm/variable records file for 5-minute data(calm_variable_records_5.dat)
c comp5fil:     Name of 1-minute and 5-minute comparison file 
c sub5fil:      Name of file listing substituted minutes
      character versn*6,stat*4,stat2*4,amonths(12)*10,inpfile*90,
     +  infiles*250,outfil*90,errfil*90,logfil*90,sumfil*90,goodfil*90,
     +  suspfil*90,origdate*12,origdate5*12,stats(10)*4,compfil*250,
     +  obfiles*250,flag*1,qcflag*1,infiles5*250,goodfil5*90,errfil5*90,
     +  clmvar5*90,comp5fil*90,sub5fil*90
     
c logical variable
c ifw:        Denotes if time period is part of IFW group
c               ifw = .true. IFW commission date is in time period or before time period
c               ifw = .false. IFW commissions date is after time period or unknown
c lwban:        Denotes if WBAN number from first record of first file did not match WBAN from other file(s)
c               lwban = .true. numbers did not match, change WBAN to number from file
c               lwban = .false. numbers matched, no change
c lstat         Denotes if call sign of station differs in files
c               lstat=.true. station ids do not match
c               lstat=.false. no change
c badwban       Denotes if WBAN is not integer, stop program if true
c lkeep1:       logical variable denoting if all 1-minute files are present
c lkeep5:       logical variable denoting if all 5-minute files are present
c lbad1:        logical variable denoting a 1-minute file has a bad filename
c lbad5:        logical variable denoting a 5-minute file has a bad filename
c writesum:     logical variable denoting if AERMINUTE is to write a summary
c               file (SUMMFILE) containing statistics about each hour in the data period
c comp:         logical variable denoting to compare standard observations against
c               one minute data
c ldup:         logical variable denonting that duplicate filenames in the one minute
c               data file list have been detected
c ldup5:         logical variable denonting that duplicate filenames in the 5 minute
c               data file list have been detected
c               true=duplicates found, stop program, false=no duplicates found
c ldupob:       logical variable denonting that duplicate filenames in the standard observation
c               data file list have been detected 
c               true=duplicates found, stop program, false=no duplicates found        
c lgo:          logical variable denoting if any problems found with standard observations
c               true=okay,false=stop program  
c lnodata:      logical variable denoting that there is data within window set by 
c               start and end dates. if lnodata is true, then there is no data that was
c               read from one minute files that is within the window and program will abort
c               if lnodata is false, program will proceeed
c lnodata5:     logical variable denoting that there is 5 minute data within window set by 
c               start and end dates. if lnodata5 is true, then there is no data that was
c               read from fiv3 minute files that is within the window and program will not
c               process 5-minute data
c               if lnodata5 is false, program will proceeed
c lfive         logical variable denoting that 5-minute data will be used
c run5          logical variable denoting if 5-minute data will be procesed
c lno1dat       logical variable denoting that the one-minute data section
c               was not found in the input file and 5-minute data was found         
c writesub5     logical variable denoting to write optional 5-minute substitution
c               file
c comp15        logical variable denoting to write optional 1-minute and 5-minute
c               comparison file.
      logical ifw,lwban,lstat,lkeep1,lbad1,writesum,comp,ldup,ldupob,
     + ldup5,lgo,lnodata,lfive,lkeep5,lbad5,run5,lnodata5,badwban,
     +lno1dat,writesub5,comp15
     
c set values for file units, d2r, and trunc variables
      parameter(mainunit=12,inunit=12,ioutunit=16,ierrunit=17,
     +  ilogunit=20,isumunit=21,igdunit=22,isusunit=23,inunit5=24,
     +  ierrunit5=25,igdunit5=26,iclmvar5=27,icomp5=28,
     +  isubunit=29,d2r=3.141592654/180.0,trunc=0.0)

c assign number of days per month.  February set to 28
c                 J  F  M  A  M  J  J  A S  O  N  D
      data days /31,28,31,30,31,30,31,31,30,31,30,31/
      
c set values for version #, error file, log file, and summary file
!      parameter(versn='11269',errfil='bad_records.dat',
!      parameter(versn='11325',errfil='bad_records.dat',
!      parameter(versn='14237',errfil='bad_records.dat',
!      parameter(versn='14337',errfil='bad_records.dat',
      parameter(versn='15272',errfil='bad_records.dat',
     +  logfil='aerminute.log',goodfil='good_records.dat',
     +  suspfil='check_records.dat',goodfil5='good_records_5.dat',
     + errfil5='bad_records_5.dat',clmvar5='calm_variable_records.dat')
     
   
c allocatable arrays
c ndays:        1 dimensional array of number of days for a month dimension nmonths
c idate:        2 dimensional array of dates (YYYYMMDD) dimension nmonths x 31 days
c               these are the original dates of data before adding 1 hour to the date
c ikeephr:      3 dimensional array of integer flags denoting whether hourly wind 
c               is to be calculated.  Dimension:  nmonths x 31 days x 24 hours
c               Values: 1 = valid hour and calculate average
c                       2 = hour was read by program but does not contain at least one
c                           non-calm minute in each 1/2 of the hour
c                       9 = hour was not in one minute data files
c               hourly averages not calculated for hours with values of -2 and -9
c ikeep:        4 dimensional array of integer flags denoting how one-minute wind is to be
c               processed. Dimension nmonths x 31 days x 24 hours x 60 minutes
c               Values: 0 = calm wind speed (0 or 1 knot for non-ifw aneomemeter)
c                       1 = non-calm wind speed ( 2 knots for non-ifw aneomemeter,
c                           any value for ifw anemometer
c                       9 = minute was not in data file or bad record
c ikeep5:       4 dimensional array of integer flags denoting how 5-minute wind is to be
c               processed. Dimension nmonths x 31 days x 24 hours x 12 minutes
c               Values: 0 = calm wind speed (0 or 1 knot for non-ifw aneomemeter)
c                       1 = non-calm wind speed ( 2 knots for non-ifw aneomemeter,
c                           any value for ifw anemometer
c                       9 = minute was not in data file or bad record
c minsrc:       4 dimensional array of integer flags denoting source of 1-minute
c               data. 1=1-minute file, 5=5-minute file
c icalm:        4 dimensional array of integer flags denoting if minute is calm.
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c               Values:  1 = calm, 0 = non-calm
c icalm5:       4 dimensional array of integer flags denoting if 5-minute is calm.
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c               Values:  1 = calm, 0 = non-calm
c idirmin:      4 dimensional array of integer wind directions for each minute
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c idirmin5:     4 dimensional array of integer wind directions for each 5-minute
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c nvalid:       3 dimensional array of integer count of number of valid minutes
c               read from data files for each hour (max of 59)
c               Dimension nmonths x 31 days x 24 hours
c nvalid5:      3 dimensional array of integer count of number of 5-minute valid 
c               minutes substituted for each hour (max of 12)
c               Dimension nmonths x 31 days x 24 hours
c ncalms:       3 dimensional array of integer count of total calms by hour
c               Dimension nmonths x 31 days x 24 hours
c neven:        3 dimensional array of integer count of total even minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c neven5:       3 dimensional array of integer count of total even 5-minutes substituted 
c               by hour
c               Dimension nmonths x 31 days x 24 hours
c nevencalm:    3 dimensional array of integer count of total even calm minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c noddperm:     3 dimensional array of integer count of total odd minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c noddpermc:    3 dimensional array of integer count of total odd calm minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c nodd5:        3 dimensional array of integer count of total odd calm 5-minutes substituted
c               by hour
c               Dimension nmonths x 31 days x 24 hours
c nodd:         3 dimensional array of integer count of total odd minutes used by hour
c               Dimension nmonths x 31 days x 24 hours
c noddcalm:     3 dimensional array of integer count of total odd calm minutes used by hour
c               Dimension nmonths x 31 days x 24 hours
c dirmin:       3 dimensional array of minimum 2-minute wind direction of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c dirmax:       3 dimensional array of maximum 2-minute wind direction of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c nhours:       2 dimensional array of # of hours per month.  1st dimension is # of months
c               and second dimension has 5 values: # of total hours, # valid hours, # invalid hours,
c               # calm hours, and # of missing hours per month
c               version 15272, added 6 column to include # of 5-minute hours used
c srcmin        4 dimensional array of minimum 2-minute wind speed/direction of each hour
c               Dimension 2 x nmonths x 31 days x 24 hours (1=speed,2=direction)
c srcmax        4 dimensional array of maximum 2-minute wind speed/direction of each hour
c               Dimension 2 x nmonths x 31 days x 24 hours (1=speed,2=direction)   
      integer, allocatable, dimension(:) :: ndays
      integer, allocatable, dimension(:,:) :: idate
      integer, allocatable, dimension(:,:,:) :: ikeephr
      integer, allocatable, dimension(:,:,:,:) :: ikeep
      integer, allocatable, dimension(:,:,:,:) :: icalm
      integer, allocatable, dimension(:,:,:,:) :: idirmin
      integer, allocatable, dimension(:,:,:) :: nvalid
      integer, allocatable, dimension(:,:,:) :: ncalms
      integer, allocatable, dimension(:,:,:) :: neven
      integer, allocatable, dimension(:,:,:) :: nevencalm
      integer, allocatable, dimension(:,:,:) :: noddperm
      integer, allocatable, dimension(:,:,:) :: noddpermc
      integer, allocatable, dimension(:,:,:) :: nodd
      integer, allocatable, dimension(:,:,:) :: noddcalm
      integer, allocatable, dimension(:,:,:) :: dirmin
      integer, allocatable, dimension(:,:,:) :: dirmax
      integer, allocatable, dimension(:,:) :: nhours
c     added 15272
      integer, allocatable, dimension(:,:,:,:) :: minsrc
      integer, allocatable, dimension(:,:,:) :: nvalid5
      integer, allocatable, dimension(:,:,:) :: neven5
      integer, allocatable, dimension(:,:,:) :: nodd5
      integer, allocatable, dimension(:,:,:,:) :: srcmin
      integer, allocatable, dimension(:,:,:,:) :: srcmax
c     152725-minute variables (remove certain variables)
      
      integer, allocatable, dimension(:,:,:,:) :: ikeep5
      integer, allocatable, dimension(:,:,:,:) :: icalm5
      integer, allocatable, dimension(:,:,:,:) :: idir5min
!      integer, allocatable, dimension(:,:,:) :: nvalid5
c real allocatable
c speed:        3 dimensional array of real hourly averaged wind speeds (m/s)
c               Dimension nmonths x 31 days x 24 hours
c speed1:       4 dimensional array of real one-minute wind speeds (m/s) read from one minute files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c speed5:       4 dimensional array of real one-minute wind speeds (m/s) read from 5-minute files
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c speedkts:    4 dimensional array of real one-minute wind speeds (knots) read from one minute files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c speed5kts:    4 dimensional array of real one-minute wind speeds (knots) read from 5 minute files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c xdir:         4 dimensional array of real one-minute wind x-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 60 minutes
c ydir:         4 dimensional array of real one-minute wind y-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 60 minutes
c dirminut:     4 dimensional array of real one-minute wind wind direction in degrees
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c xdir5:        4 dimensional array of real 5-minute wind x-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 12 minutes
c ydir5:        4 dimensional array of real 5-minute wind y-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 12 minutes
c dir5minut:    4 dimensional array of real 5-minute wind wind direction in degrees
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c xdirhr:       3 dimensional array of sum of one minute x-components of wind direction
c               Dimension nmonths x 31 days x 24 hours
c ydirhr:       3 dimensional array of sum of one minute y-components of wind direction
c               Dimension nmonths x 31 days x 24 hours
c dirhr:        3 dimensional array of hourly averaged wind direction in degrees
c               Dimension nmonths x 31 days x 24 hours
c speedmin:     3 dimensional array of minimum 2-minute wind speed of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c speedmax:     3 dimensional array of maximum 2-minute wind speed of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c obspd:        4 dimensional array of real standard observation wind speeds (m/s) read from standard ob files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c obdir:        4 dimensional array of real standard observation wind directions read from standard ob files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
      real, allocatable, dimension(:,:,:) :: speed
      real, allocatable, dimension(:,:,:,:) :: speed1
      real, allocatable, dimension(:,:,:,:) :: speedkts
      real, allocatable, dimension(:,:,:,:) :: xdir
      real, allocatable, dimension(:,:,:,:) :: ydir
      real, allocatable, dimension(:,:,:,:) :: dirminut
      real, allocatable, dimension(:,:,:) :: xdirhr
      real, allocatable, dimension(:,:,:) :: ydirhr
      real, allocatable, dimension(:,:,:) :: dirhr
      real, allocatable, dimension(:,:,:) :: speedmin
      real, allocatable, dimension(:,:,:) :: speedmax
      real, allocatable, dimension(:,:,:,:) :: obspd
      real, allocatable, dimension(:,:,:,:) :: obdir      

c version 152725-minute variables

      real, allocatable, dimension(:,:,:,:) :: speed5
      real, allocatable, dimension(:,:,:,:) :: xdir5
      real, allocatable, dimension(:,:,:,:) :: ydir5
      real, allocatable, dimension(:,:,:,:) :: dir5minut
      real, allocatable, dimension(:,:,:,:) :: speed5kts
c allocatable statement for various character arrays.    
      allocatable :: infiles(:)
      allocatable :: obfiles(:)
      allocatable :: origdate(:,:,:,:)      
      allocatable :: flag(:,:,:,:)
      allocatable :: qcflag(:,:,:,:)  

c version 15272
      allocatable :: infiles5(:)
      allocatable :: origdate5(:,:,:,:)
      integer iminr,irnd(12,24,12)
c irnd:       integer values of random numbers to generate 5-minute
c             wind directions.  array size is 12 months x 24 hours x 12 minutes
c             each day of a month gets the same values.
      DATA (IRND( 1, 1,iminr),iminr=1,12)/7,0,0,8,6,0,5,8,5,0,3,4/
      DATA (IRND( 1, 2,iminr),iminr=1,12)/4,8,0,1,7,8,6,1,2,1,3,3/
      DATA (IRND( 1, 3,iminr),iminr=1,12)/6,6,5,3,6,2,7,1,5,2,4,8/
      DATA (IRND( 1, 4,iminr),iminr=1,12)/8,6,2,7,3,6,6,8,3,7,3,4/
      DATA (IRND( 1, 5,iminr),iminr=1,12)/2,4,8,1,2,0,7,7,6,8,4,0/
      DATA (IRND( 1, 6,iminr),iminr=1,12)/6,3,1,4,3,1,7,0,0,4,4,5/
      DATA (IRND( 1, 7,iminr),iminr=1,12)/1,5,8,8,7,8,7,8,3,3,8,4/
      DATA (IRND( 1, 8,iminr),iminr=1,12)/5,8,7,2,3,5,8,7,6,2,2,4/
      DATA (IRND( 1, 9,iminr),iminr=1,12)/0,1,6,6,8,2,8,6,0,1,5,4/
      DATA (IRND( 1,10,iminr),iminr=1,12)/4,6,5,2,1,1,6,3,5,4,6,4/
      DATA (IRND( 1,11,iminr),iminr=1,12)/8,0,4,5,6,7,6,1,8,3,1,4/
      DATA (IRND( 1,12,iminr),iminr=1,12)/3,2,2,0,1,4,6,0,2,2,4,4/
      DATA (IRND( 1,13,iminr),iminr=1,12)/7,5,1,3,6,2,6,8,5,1,7,3/
      DATA (IRND( 1,14,iminr),iminr=1,12)/2,8,0,7,2,8,7,7,8,0,2,3/
      DATA (IRND( 1,15,iminr),iminr=1,12)/6,3,8,3,4,7,5,4,5,3,3,4/
      DATA (IRND( 1,16,iminr),iminr=1,12)/1,6,7,6,0,4,5,3,7,2,6,3/
      DATA (IRND( 1,17,iminr),iminr=1,12)/5,0,5,1,5,1,5,2,1,1,0,3/
      DATA (IRND( 1,18,iminr),iminr=1,12)/0,2,4,4,0,8,5,1,4,0,4,3/
      DATA (IRND( 1,19,iminr),iminr=1,12)/4,7,3,0,2,6,3,7,1,3,5,3/
      DATA (IRND( 1,20,iminr),iminr=1,12)/8,1,2,4,7,4,4,6,4,2,8,3/
      DATA (IRND( 1,21,iminr),iminr=1,12)/3,3,1,7,3,1,4,5,6,1,2,3/
      DATA (IRND( 1,22,iminr),iminr=1,12)/7,6,8,2,8,7,4,4,0,1,6,2/
      DATA (IRND( 1,23,iminr),iminr=1,12)/2,2,7,7,1,6,2,0,6,3,7,3/
      DATA (IRND( 1,24,iminr),iminr=1,12)/6,4,6,1,5,3,2,8,0,2,1,3/
      DATA (IRND( 2, 1,iminr),iminr=1,12)/1,7,5,5,1,1,3,7,3,1,5,2/
      DATA (IRND( 2, 2,iminr),iminr=1,12)/5,0,4,8,6,7,3,6,6,1,8,2/
      DATA (IRND( 2, 3,iminr),iminr=1,12)/0,5,3,4,8,6,1,3,2,3,0,3/
      DATA (IRND( 2, 4,iminr),iminr=1,12)/4,8,1,8,4,3,1,2,5,2,3,2/
      DATA (IRND( 2, 5,iminr),iminr=1,12)/8,1,0,2,0,0,1,1,8,1,7,2/
      DATA (IRND( 2, 6,iminr),iminr=1,12)/3,4,8,6,4,7,1,0,2,1,1,2/
      DATA (IRND( 2, 7,iminr),iminr=1,12)/7,0,7,1,6,5,8,6,8,3,2,2/
      DATA (IRND( 2, 8,iminr),iminr=1,12)/2,2,6,5,2,3,0,5,2,2,5,2/
      DATA (IRND( 2, 9,iminr),iminr=1,12)/6,5,4,0,7,0,0,4,4,1,0,2/
      DATA (IRND( 2,10,iminr),iminr=1,12)/1,8,3,3,3,6,0,2,7,1,3,1/
      DATA (IRND( 2,11,iminr),iminr=1,12)/5,3,2,8,5,5,7,8,4,3,4,2/
      DATA (IRND( 2,12,iminr),iminr=1,12)/0,6,1,2,0,2,7,7,7,2,8,2/
      DATA (IRND( 2,13,iminr),iminr=1,12)/5,8,0,6,5,0,8,6,1,2,2,1/
      DATA (IRND( 2,14,iminr),iminr=1,12)/0,2,7,0,1,6,8,5,4,1,5,1/
      DATA (IRND( 2,15,iminr),iminr=1,12)/3,3,2,4,1,0,1,0,8,8,8,2/
      DATA (IRND( 2,16,iminr),iminr=1,12)/0,3,4,4,7,0,1,3,6,0,2,8/
      DATA (IRND( 2,17,iminr),iminr=1,12)/2,3,1,8,4,5,0,1,4,4,1,4/
      DATA (IRND( 2,18,iminr),iminr=1,12)/5,1,6,2,4,8,1,1,7,5,3,0/
      DATA (IRND( 2,19,iminr),iminr=1,12)/7,7,1,6,7,0,3,8,0,6,0,7/
      DATA (IRND( 2,20,iminr),iminr=1,12)/1,5,7,0,7,3,5,8,4,7,1,2/
      DATA (IRND( 2,21,iminr),iminr=1,12)/3,5,3,4,3,8,4,6,1,2,1,8/
      DATA (IRND( 2,22,iminr),iminr=1,12)/5,3,0,7,3,2,5,5,5,3,2,3/
      DATA (IRND( 2,23,iminr),iminr=1,12)/8,1,4,2,6,4,7,4,7,4,8,1/
      DATA (IRND( 2,24,iminr),iminr=1,12)/1,8,0,4,6,7,8,3,2,5,1,6/
      DATA (IRND( 3, 1,iminr),iminr=1,12)/4,8,6,8,2,2,7,1,8,0,0,2/
      DATA (IRND( 3, 2,iminr),iminr=1,12)/6,6,3,2,2,5,0,1,2,1,1,7/
      DATA (IRND( 3, 3,iminr),iminr=1,12)/0,4,6,6,5,7,2,8,5,2,8,5/
      DATA (IRND( 3, 4,iminr),iminr=1,12)/2,2,3,0,5,1,3,8,8,3,0,0/
      DATA (IRND( 3, 5,iminr),iminr=1,12)/4,2,8,4,2,5,2,6,6,7,8,6/
      DATA (IRND( 3, 6,iminr),iminr=1,12)/7,0,5,7,1,8,3,5,0,8,1,2/
      DATA (IRND( 3, 7,iminr),iminr=1,12)/0,7,0,2,4,1,6,4,3,0,7,8/
      DATA (IRND( 3, 8,iminr),iminr=1,12)/3,5,5,5,4,4,7,4,6,1,8,4/
      DATA (IRND( 3, 9,iminr),iminr=1,12)/5,5,2,0,1,8,6,1,3,5,8,1/
      DATA (IRND( 3,10,iminr),iminr=1,12)/8,2,8,3,0,2,7,1,7,6,0,5/
      DATA (IRND( 3,11,iminr),iminr=1,12)/1,0,2,7,3,4,1,8,0,7,6,3/
      DATA (IRND( 3,12,iminr),iminr=1,12)/3,7,8,0,3,7,2,8,4,8,8,8/
      DATA (IRND( 3,13,iminr),iminr=1,12)/6,7,5,4,0,3,1,6,1,3,7,4/
      DATA (IRND( 3,14,iminr),iminr=1,12)/8,5,1,7,8,6,2,5,4,4,8,0/
      DATA (IRND( 3,15,iminr),iminr=1,12)/2,3,5,2,2,7,4,4,7,5,6,7/
      DATA (IRND( 3,16,iminr),iminr=1,12)/4,1,1,5,2,1,5,4,1,6,7,2/
      DATA (IRND( 3,17,iminr),iminr=1,12)/7,1,7,0,8,6,4,1,8,1,6,8/
      DATA (IRND( 3,18,iminr),iminr=1,12)/0,8,4,3,7,0,6,1,2,3,8,3/
      DATA (IRND( 3,19,iminr),iminr=1,12)/3,6,7,7,2,1,8,8,5,3,5,1/
      DATA (IRND( 3,20,iminr),iminr=1,12)/5,4,4,1,1,4,0,8,8,4,7,6/
      DATA (IRND( 3,21,iminr),iminr=1,12)/7,4,1,5,7,0,8,6,6,0,6,2/
      DATA (IRND( 3,22,iminr),iminr=1,12)/1,2,6,8,6,3,0,5,0,1,7,7/
      DATA (IRND( 3,23,iminr),iminr=1,12)/8,5,6,7,5,2,8,4,8,7,7,0/
      DATA (IRND( 3,24,iminr),iminr=1,12)/3,8,5,2,1,0,8,3,1,6,1,0/
      DATA (IRND( 4, 1,iminr),iminr=1,12)/0,4,8,7,1,8,7,0,4,4,0,0/
      DATA (IRND( 4, 2,iminr),iminr=1,12)/4,7,7,1,6,5,7,8,6,3,3,0/
      DATA (IRND( 4, 3,iminr),iminr=1,12)/8,3,6,6,8,4,5,4,3,6,4,0/
      DATA (IRND( 4, 4,iminr),iminr=1,12)/3,5,5,1,4,1,6,3,6,5,7,0/
      DATA (IRND( 4, 5,iminr),iminr=1,12)/8,8,3,4,0,8,6,2,0,4,2,0/
      DATA (IRND( 4, 6,iminr),iminr=1,12)/5,4,7,1,0,7,5,7,2,2,0,0/
      DATA (IRND( 4, 7,iminr),iminr=1,12)/0,0,6,5,2,6,3,4,8,5,1,0/
      DATA (IRND( 4, 8,iminr),iminr=1,12)/4,3,4,0,7,3,3,3,2,4,4,0/
      DATA (IRND( 4, 9,iminr),iminr=1,12)/8,5,3,3,2,0,3,2,5,3,8,0/
      DATA (IRND( 4,10,iminr),iminr=1,12)/3,8,2,7,7,7,4,1,7,2,2,8/
      DATA (IRND( 4,11,iminr),iminr=1,12)/7,4,1,3,0,5,2,7,4,5,3,0/
      DATA (IRND( 4,12,iminr),iminr=1,12)/2,6,0,6,5,3,2,6,7,4,6,0/
      DATA (IRND( 4,13,iminr),iminr=1,12)/6,0,7,1,1,0,2,5,1,3,1,8/
      DATA (IRND( 4,14,iminr),iminr=1,12)/1,2,6,4,6,6,2,4,4,2,4,8/
      DATA (IRND( 4,15,iminr),iminr=1,12)/5,7,5,0,8,5,0,0,1,5,5,0/
      DATA (IRND( 4,16,iminr),iminr=1,12)/0,1,4,4,3,2,1,8,3,4,8,8/
      DATA (IRND( 4,17,iminr),iminr=1,12)/4,3,3,7,8,0,1,7,6,3,3,8/
      DATA (IRND( 4,18,iminr),iminr=1,12)/8,6,1,2,4,6,1,6,0,2,6,8/
      DATA (IRND( 4,19,iminr),iminr=1,12)/3,2,0,7,6,5,8,3,6,5,7,8/
      DATA (IRND( 4,20,iminr),iminr=1,12)/7,4,8,1,2,2,8,2,0,4,2,8/
      DATA (IRND( 4,21,iminr),iminr=1,12)/2,7,7,5,7,8,8,1,3,3,5,8/
      DATA (IRND( 4,22,iminr),iminr=1,12)/6,1,6,8,2,6,0,0,5,2,8,7/
      DATA (IRND( 4,23,iminr),iminr=1,12)/1,5,4,4,4,4,7,6,2,5,0,8/
      DATA (IRND( 4,24,iminr),iminr=1,12)/5,8,3,7,0,2,7,5,5,4,4,8/
      DATA (IRND( 5, 1,iminr),iminr=1,12)/0,2,2,2,5,8,7,4,8,3,7,7/
      DATA (IRND( 5, 2,iminr),iminr=1,12)/4,4,1,6,1,5,7,3,2,2,1,7/
      DATA (IRND( 5, 3,iminr),iminr=1,12)/8,0,0,1,3,4,5,8,8,5,2,8/
      DATA (IRND( 5, 4,iminr),iminr=1,12)/3,2,8,5,7,1,6,7,1,4,6,7/
      DATA (IRND( 5, 5,iminr),iminr=1,12)/7,5,6,8,3,8,6,6,4,3,0,7/
      DATA (IRND( 5, 6,iminr),iminr=1,12)/2,8,5,3,8,5,6,5,7,2,3,7/
      DATA (IRND( 5, 7,iminr),iminr=1,12)/6,6,6,2,1,4,8,6,6,0,4,7/
      DATA (IRND( 5, 8,iminr),iminr=1,12)/0,4,2,5,1,7,0,5,0,1,5,2/
      DATA (IRND( 5, 9,iminr),iminr=1,12)/2,4,8,0,6,2,8,3,7,5,4,8/
      DATA (IRND( 5,10,iminr),iminr=1,12)/4,2,5,3,6,5,0,3,1,6,6,4/
      DATA (IRND( 5,11,iminr),iminr=1,12)/7,0,8,7,0,7,3,1,4,7,3,1/
      DATA (IRND( 5,12,iminr),iminr=1,12)/0,7,5,0,0,1,4,1,7,8,4,6/
      DATA (IRND( 5,13,iminr),iminr=1,12)/3,7,2,4,5,5,3,7,4,3,4,3/
      DATA (IRND( 5,14,iminr),iminr=1,12)/5,5,7,7,5,8,4,7,8,4,5,7/
      DATA (IRND( 5,15,iminr),iminr=1,12)/8,2,2,2,8,1,7,6,1,5,2,5/
      DATA (IRND( 5,16,iminr),iminr=1,12)/1,0,7,5,8,4,8,5,5,6,4,1/
      DATA (IRND( 5,17,iminr),iminr=1,12)/3,0,4,0,5,8,7,3,2,1,3,6/
      DATA (IRND( 5,18,iminr),iminr=1,12)/6,7,1,3,4,2,8,3,6,2,4,2/
      DATA (IRND( 5,19,iminr),iminr=1,12)/8,5,4,7,7,4,1,1,8,3,2,0/
      DATA (IRND( 5,20,iminr),iminr=1,12)/2,3,1,1,7,7,3,1,2,4,3,4/
      DATA (IRND( 5,21,iminr),iminr=1,12)/7,4,3,2,1,0,0,2,4,8,4,2/
      DATA (IRND( 5,22,iminr),iminr=1,12)/1,2,0,5,1,3,1,2,7,0,5,6/
      DATA (IRND( 5,23,iminr),iminr=1,12)/3,0,3,0,4,5,4,0,1,1,3,4/
      DATA (IRND( 5,24,iminr),iminr=1,12)/6,7,0,3,4,8,5,0,4,2,4,0/
      DATA (IRND( 6, 1,iminr),iminr=1,12)/8,7,6,7,0,3,4,7,1,6,3,5/
      DATA (IRND( 6, 2,iminr),iminr=1,12)/1,5,2,1,0,6,5,7,5,8,5,1/
      DATA (IRND( 6, 3,iminr),iminr=1,12)/4,3,6,5,3,8,8,5,7,8,2,8/
      DATA (IRND( 6, 4,iminr),iminr=1,12)/6,1,3,7,3,2,0,5,2,0,4,3/
      DATA (IRND( 6, 5,iminr),iminr=1,12)/0,8,8,1,2,5,1,5,5,1,5,8/
      DATA (IRND( 6, 6,iminr),iminr=1,12)/2,8,5,5,8,1,0,2,3,6,4,5/
      DATA (IRND( 6, 7,iminr),iminr=1,12)/5,6,1,8,8,4,1,2,6,7,5,0/
      DATA (IRND( 6, 8,iminr),iminr=1,12)/7,4,5,3,2,5,4,0,0,7,3,7/
      DATA (IRND( 6, 9,iminr),iminr=1,12)/0,2,2,6,1,8,5,0,3,8,4,3/
      DATA (IRND( 6,10,iminr),iminr=1,12)/3,2,7,1,7,4,4,7,0,4,4,8/
      DATA (IRND( 6,11,iminr),iminr=1,12)/5,0,4,4,7,7,5,6,4,5,5,4/
      DATA (IRND( 6,12,iminr),iminr=1,12)/8,6,8,8,1,8,7,5,6,5,2,2/
      DATA (IRND( 6,13,iminr),iminr=1,12)/1,4,4,2,0,2,0,5,1,6,4,6/
      DATA (IRND( 6,14,iminr),iminr=1,12)/4,4,1,6,6,7,7,2,7,2,3,3/
      DATA (IRND( 6,15,iminr),iminr=1,12)/6,2,7,8,6,1,0,2,1,3,4,7/
      DATA (IRND( 6,16,iminr),iminr=1,12)/6,4,3,4,8,8,6,3,5,8,0,6/
      DATA (IRND( 6,17,iminr),iminr=1,12)/1,7,1,8,4,5,7,2,8,7,3,5/
      DATA (IRND( 6,18,iminr),iminr=1,12)/5,1,0,2,0,2,7,1,1,6,6,5/
      DATA (IRND( 6,19,iminr),iminr=1,12)/0,3,8,6,4,8,7,0,4,5,1,5/
      DATA (IRND( 6,20,iminr),iminr=1,12)/4,8,7,2,6,7,5,6,1,8,2,5/
      DATA (IRND( 6,21,iminr),iminr=1,12)/8,2,6,5,2,5,5,5,4,7,5,5/
      DATA (IRND( 6,22,iminr),iminr=1,12)/3,4,4,0,7,2,6,4,7,6,8,5/
      DATA (IRND( 6,23,iminr),iminr=1,12)/7,7,3,3,3,8,6,3,1,5,3,5/
      DATA (IRND( 6,24,iminr),iminr=1,12)/4,5,7,1,0,0,3,6,6,7,8,5/
      DATA (IRND( 7, 1,iminr),iminr=1,12)/8,8,5,4,5,6,3,5,0,6,2,5/
      DATA (IRND( 7, 2,iminr),iminr=1,12)/3,2,4,8,1,4,3,4,3,5,5,5/
      DATA (IRND( 7, 3,iminr),iminr=1,12)/7,4,3,2,6,1,3,3,5,4,0,4/
      DATA (IRND( 7, 4,iminr),iminr=1,12)/2,0,2,7,8,0,1,0,2,7,1,5/
      DATA (IRND( 7, 5,iminr),iminr=1,12)/6,3,1,2,3,6,2,8,5,6,4,5/
      DATA (IRND( 7, 6,iminr),iminr=1,12)/1,5,8,5,8,3,2,7,8,5,7,4/
      DATA (IRND( 7, 7,iminr),iminr=1,12)/5,8,7,0,4,0,2,5,2,4,2,4/
      DATA (IRND( 7, 8,iminr),iminr=1,12)/0,4,6,5,6,8,0,2,7,7,3,5/
      DATA (IRND( 7, 9,iminr),iminr=1,12)/4,6,5,8,2,6,0,1,1,6,6,4/
      DATA (IRND( 7,10,iminr),iminr=1,12)/8,0,4,3,6,3,0,0,4,5,0,4/
      DATA (IRND( 7,11,iminr),iminr=1,12)/6,5,7,8,7,2,8,5,6,3,8,4/
      DATA (IRND( 7,12,iminr),iminr=1,12)/1,1,6,4,0,1,6,2,3,6,0,5/
      DATA (IRND( 7,13,iminr),iminr=1,12)/5,4,4,7,5,7,7,1,6,5,3,4/
      DATA (IRND( 7,14,iminr),iminr=1,12)/0,6,3,2,0,4,7,0,0,4,6,4/
      DATA (IRND( 7,15,iminr),iminr=1,12)/4,0,2,5,5,2,7,8,3,3,1,4/
      DATA (IRND( 7,16,iminr),iminr=1,12)/8,5,1,1,7,1,5,5,0,6,2,4/
      DATA (IRND( 7,17,iminr),iminr=1,12)/3,7,0,5,3,7,5,4,2,5,5,4/
      DATA (IRND( 7,18,iminr),iminr=1,12)/7,1,7,8,8,4,6,3,5,4,8,4/
      DATA (IRND( 7,19,iminr),iminr=1,12)/2,3,6,3,3,1,6,2,8,3,3,3/
      DATA (IRND( 7,20,iminr),iminr=1,12)/6,8,5,7,5,0,4,7,5,6,4,4/
      DATA (IRND( 7,21,iminr),iminr=1,12)/1,2,4,2,1,7,4,6,8,5,7,4/
      DATA (IRND( 7,22,iminr),iminr=1,12)/5,4,3,6,6,4,4,5,1,4,2,3/
      DATA (IRND( 7,23,iminr),iminr=1,12)/0,7,2,0,2,1,5,4,4,3,5,3/
      DATA (IRND( 7,24,iminr),iminr=1,12)/2,7,6,3,0,2,7,2,7,2,1,7/
      DATA (IRND( 8, 1,iminr),iminr=1,12)/5,4,3,6,0,5,0,2,2,3,2,3/
      DATA (IRND( 8, 2,iminr),iminr=1,12)/7,4,8,1,6,0,8,8,8,7,1,8/
      DATA (IRND( 8, 3,iminr),iminr=1,12)/1,2,5,3,5,3,0,8,3,8,3,4/
      DATA (IRND( 8, 4,iminr),iminr=1,12)/3,0,8,7,8,5,2,7,5,0,0,2/
      DATA (IRND( 8, 5,iminr),iminr=1,12)/5,7,5,1,8,8,3,6,0,1,1,6/
      DATA (IRND( 8, 6,iminr),iminr=1,12)/8,7,2,5,5,3,2,4,6,5,1,3/
      DATA (IRND( 8, 7,iminr),iminr=1,12)/1,5,7,8,4,6,3,4,0,6,2,7/
      DATA (IRND( 8, 8,iminr),iminr=1,12)/4,3,2,3,8,8,6,2,3,7,8,5/
      DATA (IRND( 8, 9,iminr),iminr=1,12)/6,1,8,6,7,2,7,2,6,8,1,1/
      DATA (IRND( 8,10,iminr),iminr=1,12)/0,1,4,1,4,6,6,8,4,3,0,6/
      DATA (IRND( 8,11,iminr),iminr=1,12)/2,8,1,4,3,0,7,8,7,4,1,2/
      DATA (IRND( 8,12,iminr),iminr=1,12)/5,6,5,8,7,2,1,7,1,5,8,0/
      DATA (IRND( 8,13,iminr),iminr=1,12)/7,4,1,2,6,5,2,6,4,6,0,4/
      DATA (IRND( 8,14,iminr),iminr=1,12)/0,4,7,6,3,1,1,4,1,1,8,1/
      DATA (IRND( 8,15,iminr),iminr=1,12)/3,2,4,8,3,4,2,4,5,3,1,6/
      DATA (IRND( 8,16,iminr),iminr=1,12)/5,8,7,3,6,5,5,2,7,3,7,3/
      DATA (IRND( 8,17,iminr),iminr=1,12)/8,6,4,6,5,8,6,2,2,4,0,8/
      DATA (IRND( 8,18,iminr),iminr=1,12)/1,6,1,1,2,4,5,8,8,0,8,5/
      DATA (IRND( 8,19,iminr),iminr=1,12)/3,4,6,4,2,7,6,8,2,1,0,0/
      DATA (IRND( 8,20,iminr),iminr=1,12)/6,2,1,8,5,8,8,7,5,1,7,7/
      DATA (IRND( 8,21,iminr),iminr=1,12)/8,0,6,2,4,2,0,6,8,2,8,3/
      DATA (IRND( 8,22,iminr),iminr=1,12)/2,0,3,6,1,7,8,4,6,7,7,8/
      DATA (IRND( 8,23,iminr),iminr=1,12)/4,7,0,0,1,1,1,4,0,8,0,4/
      DATA (IRND( 8,24,iminr),iminr=1,12)/7,5,3,4,4,3,3,2,3,8,6,2/
      DATA (IRND( 9, 1,iminr),iminr=1,12)/0,3,0,7,3,6,4,2,6,0,7,6/
      DATA (IRND( 9, 2,iminr),iminr=1,12)/6,4,2,8,7,8,2,3,7,5,8,4/
      DATA (IRND( 9, 3,iminr),iminr=1,12)/8,2,8,2,6,2,3,3,2,6,1,8/
      DATA (IRND( 9, 4,iminr),iminr=1,12)/2,0,2,6,1,4,5,1,4,6,7,6/
      DATA (IRND( 9, 5,iminr),iminr=1,12)/4,7,8,0,0,7,7,1,8,7,8,2/
      DATA (IRND( 9, 6,iminr),iminr=1,12)/6,7,5,4,6,2,6,8,5,3,8,7/
      DATA (IRND( 9, 7,iminr),iminr=1,12)/0,5,1,6,6,5,7,8,0,4,0,3/
      DATA (IRND( 9, 8,iminr),iminr=1,12)/0,1,0,7,6,7,5,4,8,0,7,2/
      DATA (IRND( 9, 9,iminr),iminr=1,12)/4,4,8,1,2,4,5,3,2,0,1,2/
      DATA (IRND( 9,10,iminr),iminr=1,12)/8,6,7,5,7,1,5,1,5,8,4,2/
      DATA (IRND( 9,11,iminr),iminr=1,12)/3,0,6,0,2,7,5,0,8,7,8,1/
      DATA (IRND( 9,12,iminr),iminr=1,12)/7,5,5,4,4,6,3,6,5,0,0,2/
      DATA (IRND( 9,13,iminr),iminr=1,12)/2,7,3,8,0,4,4,5,7,0,3,2/
      DATA (IRND( 9,14,iminr),iminr=1,12)/6,1,2,2,5,1,4,4,1,8,7,1/
      DATA (IRND( 9,15,iminr),iminr=1,12)/1,4,1,6,1,7,4,3,4,7,1,1/
      DATA (IRND( 9,16,iminr),iminr=1,12)/5,8,0,2,3,6,2,0,1,1,2,2/
      DATA (IRND( 9,17,iminr),iminr=1,12)/0,2,8,5,8,3,2,8,4,0,5,1/
      DATA (IRND( 9,18,iminr),iminr=1,12)/4,4,6,0,3,0,3,7,7,8,0,1/
      DATA (IRND( 9,19,iminr),iminr=1,12)/8,7,5,3,8,7,3,6,0,7,3,1/
      DATA (IRND( 9,20,iminr),iminr=1,12)/3,3,4,8,1,6,1,2,6,1,4,1/
      DATA (IRND( 9,21,iminr),iminr=1,12)/7,5,3,3,6,3,1,1,0,0,7,1/
      DATA (IRND( 9,22,iminr),iminr=1,12)/2,8,2,6,2,0,1,0,3,8,2,1/
      DATA (IRND( 9,23,iminr),iminr=1,12)/6,2,0,1,6,6,1,8,6,7,5,0/
      DATA (IRND( 9,24,iminr),iminr=1,12)/3,0,4,7,4,7,7,2,2,8,1,1/
      DATA (IRND(10, 1,iminr),iminr=1,12)/7,3,2,2,0,4,7,1,5,8,4,1/
      DATA (IRND(10, 2,iminr),iminr=1,12)/2,5,1,5,4,2,8,0,8,7,8,1/
      DATA (IRND(10, 3,iminr),iminr=1,12)/6,8,0,0,0,8,8,8,1,6,2,0/
      DATA (IRND(10, 4,iminr),iminr=1,12)/1,4,8,5,2,7,6,5,7,8,3,1/
      DATA (IRND(10, 5,iminr),iminr=1,12)/5,6,7,8,7,4,6,4,1,8,6,1/
      DATA (IRND(10, 6,iminr),iminr=1,12)/0,0,5,3,3,1,6,3,4,7,1,0/
      DATA (IRND(10, 7,iminr),iminr=1,12)/4,3,4,6,8,8,7,2,7,6,4,0/
      DATA (IRND(10, 8,iminr),iminr=1,12)/8,7,3,2,1,7,5,8,4,8,5,1/
      DATA (IRND(10, 9,iminr),iminr=1,12)/3,1,2,6,5,4,5,7,6,8,0,0/
      DATA (IRND(10,10,iminr),iminr=1,12)/7,4,1,0,1,1,5,5,0,7,3,0/
      DATA (IRND(10,11,iminr),iminr=1,12)/2,6,0,4,6,7,5,4,3,6,6,0/
      DATA (IRND(10,12,iminr),iminr=1,12)/6,2,7,8,8,6,3,1,0,0,7,0/
      DATA (IRND(10,13,iminr),iminr=1,12)/1,5,6,3,4,3,4,0,3,8,2,0/
      DATA (IRND(10,14,iminr),iminr=1,12)/5,7,5,6,0,1,4,8,5,7,5,0/
      DATA (IRND(10,15,iminr),iminr=1,12)/0,1,4,1,4,7,4,7,8,6,8,8/
      DATA (IRND(10,16,iminr),iminr=1,12)/5,0,1,1,0,6,6,8,6,3,5,3/
      DATA (IRND(10,17,iminr),iminr=1,12)/8,7,6,4,0,0,7,8,0,4,7,7/
      DATA (IRND(10,18,iminr),iminr=1,12)/1,7,3,8,6,4,6,5,6,8,6,4/
      DATA (IRND(10,19,iminr),iminr=1,12)/3,5,8,1,5,7,7,5,1,0,7,0/
      DATA (IRND(10,20,iminr),iminr=1,12)/6,3,3,5,8,0,0,3,3,1,5,6/
      DATA (IRND(10,21,iminr),iminr=1,12)/8,1,0,8,8,3,1,3,7,2,6,2/
      DATA (IRND(10,22,iminr),iminr=1,12)/2,1,5,3,5,7,0,1,4,6,5,8/
      DATA (IRND(10,23,iminr),iminr=1,12)/4,8,2,6,4,1,2,0,7,7,7,3/
      DATA (IRND(10,24,iminr),iminr=1,12)/7,6,8,0,4,4,3,0,2,0,8,8/
      DATA (IRND(11, 1,iminr),iminr=1,12)/0,4,2,4,7,6,5,8,4,0,6,6/
      DATA (IRND(11, 2,iminr),iminr=1,12)/2,2,8,7,6,0,6,7,8,1,7,1/
      DATA (IRND(11, 3,iminr),iminr=1,12)/5,2,5,2,3,4,5,5,5,6,6,7/
      DATA (IRND(11, 4,iminr),iminr=1,12)/7,8,1,5,3,7,7,5,0,7,7,2/
      DATA (IRND(11, 5,iminr),iminr=1,12)/1,6,5,0,6,0,0,3,2,7,5,0/
      DATA (IRND(11, 6,iminr),iminr=1,12)/3,4,1,2,6,3,1,3,5,8,6,5/
      DATA (IRND(11, 7,iminr),iminr=1,12)/6,4,7,6,2,8,0,0,3,4,5,1/
      DATA (IRND(11, 8,iminr),iminr=1,12)/8,2,4,0,2,2,1,0,6,5,7,6/
      DATA (IRND(11, 9,iminr),iminr=1,12)/1,0,7,4,5,3,4,8,0,5,4,4/
      DATA (IRND(11,10,iminr),iminr=1,12)/4,7,4,7,5,6,5,7,3,6,6,8/
      DATA (IRND(11,11,iminr),iminr=1,12)/6,7,1,2,1,2,4,5,1,2,5,5/
      DATA (IRND(11,12,iminr),iminr=1,12)/0,5,6,5,1,5,5,5,4,3,6,1/
      DATA (IRND(11,13,iminr),iminr=1,12)/2,3,1,0,4,6,8,3,7,3,4,7/
      DATA (IRND(11,14,iminr),iminr=1,12)/5,1,7,3,4,0,0,3,1,4,5,3/
      DATA (IRND(11,15,iminr),iminr=1,12)/7,1,3,7,0,5,8,0,7,0,4,0/
      DATA (IRND(11,16,iminr),iminr=1,12)/0,8,0,1,0,8,0,0,2,1,6,4/
      DATA (IRND(11,17,iminr),iminr=1,12)/3,5,4,5,3,1,2,8,4,1,3,2/
      DATA (IRND(11,18,iminr),iminr=1,12)/5,3,0,8,3,4,4,7,8,2,4,7/
      DATA (IRND(11,19,iminr),iminr=1,12)/8,3,6,3,0,8,2,5,5,7,4,3/
      DATA (IRND(11,20,iminr),iminr=1,12)/1,1,3,5,8,2,4,5,8,8,5,8/
      DATA (IRND(11,21,iminr),iminr=1,12)/4,8,6,0,2,4,6,3,2,8,2,6/
      DATA (IRND(11,22,iminr),iminr=1,12)/6,6,3,3,2,7,7,3,5,0,4,1/
      DATA (IRND(11,23,iminr),iminr=1,12)/8,6,0,7,8,2,6,1,3,5,3,7/
      DATA (IRND(11,24,iminr),iminr=1,12)/2,4,5,1,7,5,7,0,6,6,4,2/
      DATA (IRND(12, 1,iminr),iminr=1,12)/4,7,1,2,4,1,4,5,6,2,5,8/
      DATA (IRND(12, 2,iminr),iminr=1,12)/8,0,0,6,0,7,5,4,0,2,8,7/
      DATA (IRND(12, 3,iminr),iminr=1,12)/3,3,8,0,5,4,5,3,3,1,2,7/
      DATA (IRND(12, 4,iminr),iminr=1,12)/7,5,7,4,1,2,5,2,6,0,5,7/
      DATA (IRND(12, 5,iminr),iminr=1,12)/2,1,6,0,3,0,3,8,2,2,7,7/
      DATA (IRND(12, 6,iminr),iminr=1,12)/6,4,4,3,8,7,3,7,5,2,1,7/
      DATA (IRND(12, 7,iminr),iminr=1,12)/3,0,8,8,8,6,2,3,8,0,8,7/
      DATA (IRND(12, 8,iminr),iminr=1,12)/8,3,6,3,4,3,3,2,1,8,2,6/
      DATA (IRND(12, 9,iminr),iminr=1,12)/3,7,5,8,6,2,1,8,7,1,4,7/
      DATA (IRND(12,10,iminr),iminr=1,12)/7,1,4,2,1,8,1,7,1,0,7,7/
      DATA (IRND(12,11,iminr),iminr=1,12)/2,4,3,6,6,6,1,6,4,0,1,6/
      DATA (IRND(12,12,iminr),iminr=1,12)/6,6,2,0,2,3,1,5,7,8,4,6/
      DATA (IRND(12,13,iminr),iminr=1,12)/1,2,0,5,4,2,8,1,3,1,6,7/
      DATA (IRND(12,14,iminr),iminr=1,12)/5,5,8,0,0,8,8,0,6,1,0,6/
      DATA (IRND(12,15,iminr),iminr=1,12)/0,7,7,3,4,5,0,8,0,0,3,6/
      DATA (IRND(12,16,iminr),iminr=1,12)/4,1,6,7,0,3,0,7,3,8,7,6/
      DATA (IRND(12,17,iminr),iminr=1,12)/8,6,5,3,2,1,7,4,0,1,8,6/
      DATA (IRND(12,18,iminr),iminr=1,12)/3,8,3,6,7,8,7,3,3,1,2,6/
      DATA (IRND(12,19,iminr),iminr=1,12)/7,2,2,1,3,5,7,2,5,0,5,6/
      DATA (IRND(12,20,iminr),iminr=1,12)/2,5,1,4,8,2,8,1,8,8,0,5/
      DATA (IRND(12,21,iminr),iminr=1,12)/6,0,0,0,1,1,6,7,5,1,1,6/
      DATA (IRND(12,22,iminr),iminr=1,12)/1,3,8,4,5,7,6,6,8,1,4,6/
      DATA (IRND(12,23,iminr),iminr=1,12)/7,8,2,0,6,7,5,2,1,8,2,6/
      DATA (IRND(12,24,iminr),iminr=1,12)/2,2,1,3,1,4,5,1,4,7,6,5/


c     array of month names
      data amonths /'January','February','March','April','May','June',
     &  'July','August','September','October','November','December'/
      end module main1

