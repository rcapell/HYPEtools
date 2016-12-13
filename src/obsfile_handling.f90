
!-----------------------------------------------------------------------------------------------------
!
! This is a collection of subroutines to access HYPE's large input text files, Pobs.txt and Tobs.txt
!
! In here:
!
!   - count_rows(funit, infile, n); count number of rows in an input text file
!       + funit INT:        unit number of file connection
!       + infile CHAR(800): file name string
!       + n INT:            number of rows
!
!   - count_datestring_len(funit, infile, dslen, tslen)
!
!   - count_data_cols(funit,infile, ncols,n_Result)
!     FROM HYPE CODE, BY CHARLOTTA PERS
!
!   - wmean(funit, infile, sbd, weight, m, nc, nr, dslen, tslen, date, time, res)
!
!
!   To compile with Intel Fortran compiler (in Win), use this command:
!   ifort /dll obsfile_handling.f90
!   Then rename to RHYPE.dll and place in ..\R\win-library\3.0\RHYPE\libs\x64
!-----------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------
SUBROUTINE count_rows(infile, infile_len, nr)
    !DEC$ ATTRIBUTES DLLEXPORT :: count_rows
    !DEC$ ATTRIBUTES ALIAS : "count_rows" :: count_rows
    ! arument declaration
    IMPLICIT NONE
    INTEGER, INTENT(in) :: infile_len
    CHARACTER(LEN = infile_len), INTENT(in) :: infile
    INTEGER, INTENT(out) :: nr
        
    nr = 0
    OPEN(unit = 10, file = infile, status = 'old', action = 'read', form = 'formatted')
    DO
        READ(10, *, END = 20)
        nr = nr + 1
        
        ! allow R to interrupt
        IF (MODULO(nr, 50) == 0) THEN
            CALL rchkusr
        END IF
    
    END DO
20  CLOSE(10)
    
END SUBROUTINE
    
    
    
    
!-----------------------------------------------------------------------------------------------------
SUBROUTINE count_data_cols(infile, infile_len, ncols)
    
    !DEC$ ATTRIBUTES DLLEXPORT :: count_data_cols
    !DEC$ ATTRIBUTES ALIAS : "count_data_cols" :: count_data_cols

    !Argument declarations
    INTEGER, INTENT(IN)  :: infile_len           !<Unit for file
    CHARACTER (LEN=infile_len), INTENT(IN) :: infile !<Name of file to be read
    INTEGER, INTENT(OUT) :: ncols           !<Total number of columns
    
    !Local variables
    CHARACTER(LEN=1700000) line
    INTEGER i,j
    INTEGER linelen
    
    !Start of subroutine
    OPEN(UNIT = 10,FILE = infile, STATUS = 'old', ERR=610)
    
    
    !Read header row and calculate number of columns
    READ(10,601) line
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
    
    CLOSE(10)
    
601 FORMAT(A1700000)
    RETURN
610 WRITE(6,*) 'ERROR open file: ', TRIM(infile)
    RETURN
    
    END SUBROUTINE count_data_cols
    
    
    
!-----------------------------------------------------------------------------------------------------
SUBROUTINE count_datestring_len(infile, infile_len, dslen, tslen, lclen)
    
    !DEC$ ATTRIBUTES DLLEXPORT :: count_datestring_len
    !DEC$ ATTRIBUTES ALIAS : "count_datestring_len" :: count_datestring_len

    !argument declaration
        INTEGER, INTENT(IN)  :: infile_len           !<Unit for file
        CHARACTER (LEN=infile_len), INTENT(IN) :: infile !<Name of file to be read
        INTEGER, INTENT(out) :: dslen, tslen, lclen    ! date, time string lengths, leading characters before datestring
        ! local variables
        CHARACTER(LEN=1700000) line
        INTEGER i,n
    
    
        OPEN(UNIT = 10,FILE = infile, STATUS = 'old')
        READ(10, *) line         ! header line, skipping
        READ(10, '(A1700000)') line
        n = 17                      ! maximum length of allowed time strings, ie yyyy-mm-dd HH:MM, plus 1
        
        ! find start of the date string, there can be leading blanks and other characters
        lclen = 0
        DO
            IF (line(lclen+1:lclen+1)==CHAR(49) .OR. line(lclen+1:lclen+1)==CHAR(50)) THEN
                EXIT
            ELSE
                lclen = lclen+1
            END IF
        END DO
        
        ! calculate date and time string lengths
        DO i = 1+lclen, n+lclen
            IF (line(i:i)==CHAR(9)) THEN
                !tab, end of date string reached
                dslen = i - 1 - lclen
                tslen = 0
                EXIT
            ELSE IF (line(i:i)==CHAR(32)) THEN
                ! space, can be either end or space between date and time
                IF (line(i+3:i+3)==CHAR(58)) THEN
                    ! a colon three characters on, which means there is time information
                    dslen = i - 1 - lclen
                    tslen = 5
                ELSE
                    dslen = i - 1 - lclen
                    tslen = 0
                    EXIT
                END IF
            END IF
            CLOSE(10)
        END DO
    
END SUBROUTINE
    
    
    
!-----------------------------------------------------------------------------------------------------
SUBROUTINE wmean(infile, infile_len, sbd, weight, m, nc, nr, tslen, res)
    !DEC$ ATTRIBUTES DLLEXPORT :: wmean
    !DEC$ ATTRIBUTES ALIAS : "wmean" :: wmean

    ! argument declaration
    CHARACTER(LEN = infile_len), INTENT(in) :: infile       ! file location string for the obs file
    INTEGER, INTENT(in) :: sbd(m)
    REAL(8), INTENT(in) :: weight(m)
    INTEGER, INTENT(in) :: infile_len, m, nc, nr, tslen       ! file unit, string length of infile, number of subcatchments of interest, no. of cols and rows in obs file, date and time string length
    REAL(8), INTENT(out) :: res(nr-1)
        
    ! local variables
    INTEGER :: pos(m), i, j          ! pos=position of subid in obs file
    CHARACTER(4) :: dummy
    CHARACTER(10) :: ddummy
    CHARACTER(5) :: tdummy
    INTEGER :: obsid(nc-1)
    REAL(8) :: obs(nc-1)
    REAL(8) :: lobs(m)          ! local obs with just the obs of interest
    REAL(8) :: wm
        
    ! read header line and match subids of interest positions therein
    OPEN(unit = 10, file = infile, status = 'old', action = 'read', form = 'formatted')
    READ(10, *) dummy, obsid
    DO j=1, m
        DO i=1, nc-1
            IF (sbd(j) == obsid(i)) THEN
                pos(j) = i
                EXIT
            END IF
        END DO
    END DO
    
    ! read file line by line and calculate weighted average, conditional on existence of time column
    IF (tslen == 0) THEN        ! just a date
        DO i = 1, nr-1
            !READ(unit = funit, fmt = *) date(i), obs(1:nc-1)
            READ(unit = 10, fmt = *) ddummy, obs
            lobs = obs(pos)
            wm = 0.
            DO j = 1, m
                wm = wm + lobs(j) * weight(j)
            END DO
            res(i) = wm
            
            ! allow R to interrupt
            IF (MODULO(i, 500) == 0) THEN
                CALL rchkusr
           END IF
            
        END DO
    ELSE                        ! both date and time
        DO i = 1, nr-1
            READ(unit = 10, fmt = *) ddummy, tdummy, obs
            lobs = obs(pos)
            wm = 0.
            DO j = 1, m
                wm = wm + lobs(j) * weight(j)
            END DO
            res(i) = wm
            
            ! allow R to interrupt
            IF (MODULO(i, 500) == 0) THEN
                CALL rchkusr
            END IF
            
        END DO
    END IF
        
    CLOSE(10)
        
END SUBROUTINE