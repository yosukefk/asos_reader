c*************************************************************
      subroutine upcase(flag)
c     convert lowercase variable to upper case variable

c flag:       variable to checked
c lower:      string of lowercase letters
c upper:      string of uppercase letters
c i:          index of flag in lower
      integer i
      character flag*1
      character lower*26,upper*26
      i=0
      lower='abcdefghijklmnopqrstuvwxyz'
      upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      
      i=index(lower,flag)

      if (i .gt. 0) flag=upper(i:i)
      return
      end
