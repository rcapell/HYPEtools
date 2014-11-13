
!-----------------------------------------------------------------------------------------------------
!
! This is a collection of subroutines to access HYPE's large input text files, Pobs.txt and Tobs.txt
!
! In here:
!
!   - nrows(funit, infile, n); count number of rows in an input text file
!       + funit INT:        unit number of file connection
!       + infile CHAR(800): file name string
!       + n INT:            number of rows
!
!   - count_datestring_len(funit, infile, dslen, tslen)
!
!   - count_data_cols(funit,infile, ncols,n_Result)
!     FROM HYPE CODE, BY CHARLOTTA PERS
!
!   - wmean(funit, infile, subid, weight, m, nc, nr, dslen, tslen, date, time, res)
!
!-----------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------
SUBROUTINE nrows(funit, infile, n)
    IMPLICIT NONE
    INTEGER, INTENT(out) :: n
    INTEGER, INTENT(in) :: funit
    CHARACTER(LEN = 800), INTENT(in) :: infile
    
    ! local variables
    integer :: ios
    CHARACTER(16) :: dummy
    
    n = 0
    OPEN(unit = 10, file = infile, status = 'old', action = 'read', form = 'formatted')
    DO
        READ(unit = funit, fmt = *, iostat = ios) dummy
        if (ios < 0) exit
        n = n + 1
    END DO
    
    CLOSE(funit)
END SUBROUTINE nrows

    
!-----------------------------------------------------------------------------------------------------
SUBROUTINE count_datestring_len(funit, infile, dslen, tslen)
    
        !argument declaration
        INTEGER, INTENT(IN)  :: funit           !<Unit for file
        CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of file to be read
        INTEGER, INTENT(out) :: dslen, tslen    ! date, time string lengths
        ! local variables
        CHARACTER(LEN=1700000) line
        INTEGER i,n
    
    
        OPEN(UNIT = funit,FILE = infile, STATUS = 'old')
        READ(funit, *) line         ! header line, skipping
        READ(funit, '(A1700000)') line
        n = 17                      ! maximum length of allowed time strings, ie yyyy-mm-dd HH:MM, plus 1
    
        DO i = 1, n
            !print *, i, line(i:i), line(i+3:i+3), CHAR(58)
            IF (line(i:i)==CHAR(9)) THEN
                !tab, end of date string reached
                dslen = i - 1
                tslen = 0
                EXIT
            ELSE IF (line(i:i)==CHAR(32)) THEN
                ! space, can be either end or space between date and time
                IF (line(i+3:i+3)==CHAR(58)) THEN
                    ! a colon three characters on, which means there is time information
                    dslen = i - 1
                    tslen = 4
                ELSE
                    dslen = i - 1
                    tslen = 0
                    EXIT
                END IF
            END IF
            CLOSE(funit)
        END DO
    
END SUBROUTINE


!-----------------------------------------------------------------------------------------------------
SUBROUTINE count_data_cols(funit,infile, ncols,n_Result)

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit           !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of file to be read
    !INTEGER, INTENT(IN)  :: nskip           !<Number of comment rows to skip in the beginning
    INTEGER, INTENT(OUT) :: ncols           !<Total number of columns
    INTEGER, INTENT(OUT) :: n_Result        !<Error number
    
    !Local variables
    CHARACTER(LEN=1700000) line
    INTEGER i,j
    INTEGER linelen

    !Start of subroutine
    n_Result = 0
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old', ERR=610)

    !IF(nskip>0) THEN !Skips nskip rows with comments
    !   DO i=1,nskip
    !      READ(funit,*) line                             
    !   ENDDO
    !ENDIF

    !Read row and calculate number of columns
    READ(funit,601) line
    j=0
    linelen = LEN_TRIM(line)
    DO i = 1,linelen
        IF(line(i:i)==CHAR(32).OR.line(i:i)==CHAR(9).OR.line(i:i)==CHAR(13).OR.line(i:i)==CHAR(10))THEN
            !skip - space, tab, CR or LF
        ELSE
            IF((line(i+1:i+1)==CHAR(32).OR.line(i+1:i+1)==CHAR(9).OR.   &
                line(i+1:i+1)==CHAR(13).OR.line(i+1:i+1)==CHAR(10)) .AND.     & 
                ((line(i:i)>=CHAR(48).AND.line(i:i)<=CHAR(57)).OR.(line(i:i)>=CHAR(65)   &
                .AND.line(i:i)<=CHAR(90)) .OR.  &
                (line(i:i)>=CHAR(97).AND.line(i:i)<=CHAR(122))) ) THEN
                j=j+1              !this char is number or letter and next is tab,space,CR or LF
            ENDIF
        ENDIF
    ENDDO
    ncols = j 

    CLOSE(funit)

601 FORMAT(A1700000)
    RETURN
610 WRITE(6,*) 'ERROR open file: ', TRIM(infile)
    n_Result = 1
    RETURN

END SUBROUTINE count_data_cols


!-----------------------------------------------------------------------------------------------------
SUBROUTINE wmean(funit, infile, subid, weight, m, nc, nr, dslen, tslen, date, time, res)
    ! argument declaration
    CHARACTER(LEN = 800), INTENT(in) :: infile       ! file location string for the obs file
    INTEGER, INTENT(in) :: subid(m)
    REAL(8), INTENT(in) :: weight(m)
    INTEGER, INTENT(in) :: funit, m, nc, nr, dslen, tslen       ! file unit, number of subcatchments of interest, no. of cols and rows in obs file, date and time string length
    CHARACTER(len=dslen), INTENT(out) :: date(nr)
    CHARACTER(len=tslen), INTENT(out) :: time(nr)
    REAL(8), INTENT(out) :: res(nr)
        
    ! local variables
    INTEGER :: n, pos(m), i, j, dscol          ! pos=position of subid in obs file
    CHARACTER(4) :: dummy
    INTEGER :: obsid(nc-1)
    REAL(8) :: obs(nc-1)
    REAL(8) :: wobs(m)
        
    ! read header line and match subids of interest positions therein
    OPEN(unit = funit, file = infile, status = 'old', action = 'read', form = 'formatted')
    READ(10, *) dummy, obsid
    DO j=1, m
        DO i=1, nc-1
            IF (subid(j) == obsid(i)) THEN
                pos(j) = i
                EXIT
            END IF
        END DO
    END DO
        
    print *, obsid(pos)
    print *, pos
        
    ! read file line by line and calculate weighted average, conditional on existence of time column
    IF (tslen == 0) THEN        ! just a date
        DO i = 1, nr-1
            READ(unit = funit, fmt = *) date(i), obs(1:nc-1)
            wobs(:) = obs(pos) * weight(:)
            res(i) = SUM(wobs)
            IF (i <= 10) THEN
                !print *, obsid(pos)
                print *, res(i)
                !print *, wobs
                !print *,obs(pos)
            END IF
        END DO
    ELSE                        ! both date and time
        DO i = 1, nr
            READ(unit = funit, fmt = *) date(i), time(i), obs(1:nc-1)
            wobs(:) = obs(pos) * weight(:)
            res(i) = SUM(wobs)
        END DO
    END IF
        
    CLOSE(10)
        
END SUBROUTINE

