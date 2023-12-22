c****************************************************
      subroutine checkfield(a,iflag)
c     check answers to prompts to determine if response has invalid
c     character, i.e. response is <return> or numeric
c     routine also processes variables that should be character length 1
      use main1
      implicit none

c a:        response entered by user
c b:        left-justified version of a
c lflag:    logical variable denoting if response is bad (true) or acceptable (false)
c lblank:   logical variable denoting if response is all blanks (true) or not (false)
c ldecimal: 
c i:        integer code of character in ASCII table  
c i1:       loop counter for character string a
c idash:    number of dashes read from response
c idecimal: number decimals read from response
c iblank:   number of blanks read from response
c icode:    code indicating what type of final response is
c           icode=1 then final answer is a character, such as a Y or N, or blank
c           icode=2 then final answer is a number
c ie:       number of upper or lowercase E's in string.  E represents exponential format
c iplus:    number of plus signs read from response
c ichar1:   2 element array of number of characters before and after blank
c charstr:  15 element array of integer ASCII codes of response

c lenb:       length of string b without blanks
      character a*84,b*84
      logical lflag,lblank
      integer i,i1,idash,idecimal,iblank,icode,ie,iplus,
     + ichar1(2),charstr(84),lenb,itime,iflag
     
  
c initialize variables
      idash=0
      ie=0
      iplus=0
      ichar1=0
      idecimal=0
      iblank=0
      charstr=32
      lflag=.false.
      lblank=.false.
      lenb=0


      b=trim(adjustl(a))
      lenb=len_trim(b)
      if (lenb .eq. 0) then
        lblank=.true.
        goto 100
      else 
        do i1=1,lenb
          call upcase(b(i1:i1))  !convert to upper case
        enddo
      endif
      

        do i1=1,lenb
          i=ichar(b(i1:i1))
          charstr(i1)=i
          if ((i .ge. 48 .and. i .le. 57) .or. i .eq. 45 .or. i .eq. 46 
     +    .or. i .eq. 69 .or. i .eq. 43) then
            if (i .eq. 45) idash=idash+1
            if (i .eq. 46) then 
              idecimal=idecimal+1
              if (ie .gt. 0)lflag=.true.
            endif
            if (i .eq. 69) ie=ie+1
            if (i .eq. 43) iplus=iplus+1
c          lflag=.false.
          else
            lflag=.true.
          endif
          if (idash .gt. 2 .or. idecimal .gt. 1 .or. ie .gt. 1 .or. 
     +    iplus .gt. 2)! .or. (ichar1(1) .ne. 0 .and. ichar1(2) .ne. 0)) 
     +    lflag=.true.
          
        enddo


     
        if (lflag) goto 100
c     need to look for specific placements of characterics if there were no bad characters
c         if first character is an e or last character is not a blank, number, or e
c         then string is bad
        if ((charstr(1) .eq. 69) .or. 
     +  ((charstr(lenb) .lt. 48 .and. charstr(lenb) .ne. 46)
     +   .or. (charstr(lenb) .gt. 57 .and. charstr(lenb) .ne. 69))) then
          lflag=.true.
          goto 100
        endif
c       check for a blank character between the first and last occurrence of non-blank character
        i=1 
        do while (i .le. lenb .and. .not. lflag)
          if (charstr(i) .eq. 32) lflag=.true.
          i=i+1
        enddo
        if (lflag) goto 100        
c       begin looking at the string beginning with position 2 through position 14
        i=2
        do while (i .le. lenb-1 .and. .not. lflag)
c           if character is an E, make sure that character before that is
c           a number or . sign and character afterwards is a number
c           or + or - sign or blank
          if (charstr(i) .eq. 69) then  !character is an e
            if (((charstr(i-1) .lt. 48 .and. charstr(i-1) .ne. 46) 
     +      .or. charstr(i-1) .gt. 57) .or. (charstr(i+1) .lt. 48 
     +      .and. charstr(i+1) .ne. 43 .and. charstr(i+1) .ne. 45)
     +      .or. charstr(i+1) .gt. 57) 
     +      lflag=.true. 
          endif     
c         if character is a - sign, preceding character should be a
c         number or blank or lower/uppercase e and proceeding character
c         should be a number
          if (charstr(i) .eq. 45 .and. (charstr(i-1) .ne. 69 .or. 
     +    (charstr(i+1) .lt. 48 .or. charstr(i+1) .gt. 57)))lflag=.true.      
c         if character is a + sign, preceding character should be a
c         number or blank or lower/uppercase e and proceeding character
c         should be a number
          if (charstr(i) .eq. 43 .and. (charstr(i-1) .ne. 69 .or. 
     +    (charstr(i+1) .lt. 48 .or. charstr(i+1) .gt. 57)))lflag=.true. 
          i=i+1
        enddo

c     check to see if number is a possible time.
c     this would be a number with no decimals, no plus signs, no dashes, no letter E and
c     length is at least 4 characters
      if (.not. lflag) then
        if (idash .eq. 0 .and. idecimal .eq. 0 .and. ie .eq. 0 .and. 
     +    iplus .eq. 0 .and. lenb .ge. 4) then  !number is an integer and could be a possible time
!          read(b,*)itime
!          if (itime .gt. 0 .and. itime .le. 2359) iflag=1
           iflag=1
        endif
      endif
          
100   return
      end
