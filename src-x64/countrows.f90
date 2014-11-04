
!IMPLICIT NONE
!subroutine nrows(n, )
!integer nlines

!nlines = 0
!OPEN (lunit, file = 'whatever')
!DO
!    READ (lunit,*, END=10)
!    nlines = nlines + 1
!END DO
!10 CLOSE (lunit)

PROGRAM countrows
  IMPLICIT NONE
  CHARACTER(LEN = 200) :: loc
  integer :: n
  
  loc = 'D:/e-hype/2014_ehype3.0/model2014-06-11/Pobs.txt'
  
  CALL nrows(n, loc)
  
  print *, loc
  print *, n
  
  CONTAINS
  
  SUBROUTINE nrows(n, loc)
    IMPLICIT NONE
    INTEGER, INTENT(out) :: n
    CHARACTER(LEN = 200), INTENT(in) :: loc
    integer :: ios
    
    n = 0
    OPEN(unit = 10, file = loc, status = 'old', action = 'read', form = 'formatted')
    DO
      READ(unit = 10, fmt = *, iostat = ios)
      n = n + 1
      print *, n, ios
      if (ios < 0) exit
    END DO
    CLOSE(10)
   END SUBROUTINE nrows
   
END PROGRAM
