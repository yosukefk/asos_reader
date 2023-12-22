      program asos1reader
        use main1
        implicit none
        ! control
        open(ilogunit, file=logfil, status='unknown')
        startdate=20000326
        enddate=20000409



        ! data
        startyear=2000
        startmon=3
        endyear=2000
        endmon=4
        !nmonths = 12
        nfiles = 12
        allocate(infiles(nfiles))
!        allocate(idate(nmonths, 31))
!        allocate(origdate(nmonths,31,24,60))
!        allocate(speed1(nmonths,31,24,60))
!        allocate(speedkts(nmonths,31,24,60))
!        allocate(ikeep(nmonths,31,24,60))
!        allocate(icalm(nmonths,31,24,60))
!      allocate(xdir(nmonths,31,24,60))
!      allocate(ydir(nmonths,31,24,60))
!      allocate(dirminut(nmonths,31,24,60))
!      allocate(idirmin(nmonths,31,24,60))
        
        infiles(1) =  'testdata/64050kpit200001.dat'
        infiles(2) =  'testdata/64050kpit200002.dat'
        infiles(3) =  'testdata/64050kpit200003.dat'
        infiles(4) =  'testdata/64050kpit200004.dat'
        infiles(5) =  'testdata/64050kpit200005.dat'
        infiles(6) =  'testdata/64050kpit200006.dat'
        infiles(7) =  'testdata/64050kpit200007.dat'
        infiles(8) =  'testdata/64050kpit200008.dat'
        infiles(9) =  'testdata/64050kpit200009.dat'
        infiles(10) = 'testdata/64050kpit200010.dat'
        infiles(11) = 'testdata/64050kpit200011.dat'
        infiles(12) = 'testdata/64050kpit200012.dat'
        call initial

        call readone
      end program
