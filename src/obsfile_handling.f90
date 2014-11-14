
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
SUBROUTINE count_rows(funit, infile, infile_len, n)
    ! arument declaration
    IMPLICIT NONE
    INTEGER, INTENT(in) :: funit, infile_len
    CHARACTER(LEN = infile_len), INTENT(in) :: infile
    INTEGER, INTENT(out) :: n
    
    ! local variables
    INTEGER :: ios
    CHARACTER(16) :: dummy
    
    n = 0
    OPEN(unit = 10, file = infile, status = 'old', action = 'read', form = 'formatted')
    DO
        !READ(unit = funit, fmt = *, iostat = ios) dummy
        READ(unit = funit, fmt = *, END = 10)
        !IF (ios < 0) EXIT
        n = n + 1
    END DO
10  CLOSE(funit)
    !CLOSE(funit)
END SUBROUTINE

    
!-----------------------------------------------------------------------------------------------------
SUBROUTINE count_datestring_len(funit, infile, infile_len, dslen, tslen)
    
        !argument declaration
        INTEGER, INTENT(IN)  :: funit, infile_len           !<Unit for file
        CHARACTER (LEN=infile_len), INTENT(IN) :: infile !<Name of file to be read
        INTEGER, INTENT(out) :: dslen, tslen    ! date, time string lengths
        ! local variables
        CHARACTER(LEN=1700000) line
        INTEGER i,n
    
    
        OPEN(UNIT = funit,FILE = infile, STATUS = 'old')
        READ(funit, *) line         ! header line, skipping
        READ(funit, '(A1700000)') line
        n = 17                      ! maximum length of allowed time strings, ie yyyy-mm-dd HH:MM, plus 1
    
        DO i = 1, n
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
SUBROUTINE count_data_cols(funit,infile, infile_len, ncols,n_Result)

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit, infile_len           !<Unit for file
    CHARACTER (LEN=infile_len), INTENT(IN) :: infile !<Name of file to be read
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

    !Read header row and calculate number of columns
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
!SUBROUTINE wmean(funit, infile, infile_len, subid, weight, m, nc, nr, dslen, tslen, date, time, res)
SUBROUTINE wmean(funit, infile, infile_len, subid, weight, m, nc, nr, dslen, tslen, res)
    ! argument declaration
    CHARACTER(LEN = infile_len), INTENT(in) :: infile       ! file location string for the obs file
    INTEGER, INTENT(in) :: subid(m)
    REAL(8), INTENT(in) :: weight(m)
    INTEGER, INTENT(in) :: funit, infile_len, m, nc, nr, dslen, tslen       ! file unit, string length of infile, number of subcatchments of interest, no. of cols and rows in obs file, date and time string length
    !CHARACTER(len=dslen), INTENT(out) :: date(nr-1)
    !CHARACTER(len=tslen), INTENT(out) :: time(nr-1)
    REAL(8), INTENT(out) :: res(nr-1)
        
    ! local variables
    INTEGER :: n, pos(m), i, j, dscol          ! pos=position of subid in obs file
    CHARACTER(4) :: dummy
    CHARACTER(10) :: ddummy
    CHARACTER(5) :: tdummy
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
        
    ! read file line by line and calculate weighted average, conditional on existence of time column
    IF (tslen == 0) THEN        ! just a date
        DO i = 1, nr-1
            !READ(unit = funit, fmt = *) date(i), obs(1:nc-1)
            READ(unit = funit, fmt = *) ddummy, obs(1:nc-1)
            wobs(:) = obs(pos) * weight(:)
            res(i) = SUM(wobs)
        END DO
    ELSE                        ! both date and time
        DO i = 1, nr-1
            READ(unit = funit, fmt = *) ddummy, tdummy, obs(1:nc-1)
            !READ(unit = funit, fmt = *) date(i), time(i), obs(1:nc-1)
            wobs(:) = obs(pos) * weight(:)
            res(i) = SUM(wobs)
        END DO
    END IF
        
    CLOSE(10)
        
END SUBROUTINE

