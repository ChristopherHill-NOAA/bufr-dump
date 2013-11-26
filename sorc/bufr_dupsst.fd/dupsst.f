C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPSST
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2013-01-13
C
C ABSTRACT: PROCESSES SEA-SURFACE TEMPERATURE DATABASE REPORTS WITH
C   DUPLICATE CHECKING AND TRIMMING TO EXACT TIME WINDOW (DAY DOWN TO
C   MINUTE).  THE ALGORITHM SORTS THE REPORTS IN ASCENDING ORDER OF
C   LAT, LON, SST VALUE, SST TYPE AND SST OBS SOURCE.  IN THE DUPLICATE
C   CHECKING, REPORTS ARE CHECKED FOR LAT, LON, SST VALUE AND SST TYPE
C   ALL INSIDE THE TOLERANCE LIMITS.  THE FILE PATH/NAMES OF THE INPUT
C   AND OUTPUT FILES, (OPTIONALLY) THE TIME WINDOW TO TRIM TO AND
C   (OPTIONALLY) DEFAULT OVERRIDE DUP CHECKING TOLERANCE LIMITS ARE
C   READ FROM STANDARD INPUT AT THE START OF THIS PROGRAM.  IF THE TIME
C   WINDOW RECORD IS MISSING, THEN NO TIME WINDOWING IS PERFORMED.  ALL
C   FILE CONNECTIONS (EXCEPT STANDARD INPUT WHICH IS PRE-CONNECTED) ARE
C   MADE THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C     8 BYTE STORAGE
C 2000-06-21  J. WOOLLEN  ENLARGED FOR 1 MILLION REPORTS
C 2000-10-13  D. KEYSER   INCREASED LIMIT FOR I/O FILENAME LENGTH
C     FROM 80 CHARACTERS TO 500 CHARACTERS; ADDED SATELLITE ID
C     (MNEMONIC "SSTSRC") TO SORT ALGORITHM LIST, WAS INADVERTANTLY
C     REMOVED IN A PREVIOUS IMPLEMENTATION (BUT ARRAY VALUE HAD STILL
C     BEEN REFERRED TO IN SORT)
C 2003-02-12  D. KEYSER   ADDED INLINE VERSION OF BUFRLIB ROUTINE
C     UFBTAB WHICH WILL NOT ABORT WHEN THERE ARE TOO MANY REPORTS
C     (> MXTB) INPUT - RATHER IT WILL JUST PROCESS MXTB REPORTS BUT
C     PRINT A DIAGNOSTIC (NOTE: THIS IS A TEMPORARY CHANGE UNTIL THE
C     NEXT VERSION OF THE BUFRLIB IS IMPLEMENTED WITH THE UPDATED
C     UFBTAB); MODIFICATIONS TO PREVENT ARRAY OVERFLOW WHEN THERE ARE >
C     MXTB REPORTS; IMPROVED DIAGNOSTIC PRINT; ADDED CALL TO
C     COMPRESS_CHECK TO INDICATE IF INPUT/OUTPUT FILES ARE COMPRESSED
C     OR UNCOMPRESSED; INCREASED MAXIMUM NUMBER OF REPORTS THAT CAN BE
C     HANDLED FROM 1 TO 3 MILLION
C 2003-09-02  D. KEYSER   REMOVED INLINE VERSION OF UFBTAB (CHANGES
C     NOTED IN PREVIOUS IMPLEMENTATION ARE NOW IN BUFRLIB VERSION);
C     REPLACED CALL TO IN-LINE SUBROUTINE COMPRESS_CHECK WITH CALL TO
C     NEW BUFRLIB ROUTINE MESGBC
C 2006-03-02  D. KEYSER   CHECKS TO SEE IF INPUT BUFR FILE CONTAINS
C     "DUMMY" MESSAGES CONTAINING DUMP CENTER TIME AND PROCESSING TIME,
C     RESP. IN FIRST TWO MESSAGES OF INPUT FILE (AFTER TABLE MSGS) BY
C     CHECKING THE VALUE OF DUMPJB SCRIPT VARIABLE "DUMMY_MSGS" (READ
C     IN VIA "GETENV") - IF NOT WILL NOT PROCESS INPUT BUFR MESSAGES
C     WITH ZERO SUBSETS AND WILL CALL BUFRLIB ROUTINE CLOSMG WITH A
C     NEGATIVE UNIT NUMBER ARGUMENT PRIOR TO ALL PROCESSING IN ORDER TO
C     SIGNAL IT THAT ANY OUTPUT BUFR MESSAGES WITH ZERO SUBSETS SHOULD
C     BE SKIPPED (NOT WRITTEN OUT) - CODE HAD BEEN HARDWIRED TO ALWAYS
C     ASSUME DUMMY MESSAGES WERE IN THE INPUT FILE; ACCOUNTS FOR
C     PHYSICAL SST RETRIEVALS IN NC012017 AND NC012018 WHICH KEY ON
C     "SAID" IN DUP- CHECK RATHER THAN "SSTSRC"; IF TIME WINDOW RECORD
C     (LINE 3) IS EMPTY IN STANDARD INPUT, TIME WINDOW TRIMMING LOOP
C     BYPASSED, BASED ON TEST FOR DEFAULT VALUE OF LATEST REQUESTED
C     DATE WHICH IS NOW CORRECTED TO BE 99999999.00_8 (WAS 99999999.00
C     BUT IN 4-BYTE REAL MACHINES THIS COULD BE ROUNDED UP TO
C     100000000.00 MEANING LASTEST DDHH.hh IN TIME TESTS, BEFORE TIME
C     WINDOW TRIMMING LOOP WAS BYPASSED, WOULD BE 0000 AND ALL REPORTS
C     WOULD BE TRIMMED RATHER THAN RETAINED)
C 2007-03-23  D. KEYSER   INTRODUCED ALLOCATABLE ARRAYS TO AVOID ARRAY
C     OVERFLOW PROBLEMS, DETERMINES SIZE OF ARRAYS BY CALLING UFBTAB
C     WITH NEGATIVE UNIT NUMBER TO SIMPLY COUNT SUBSETS
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- ADAPTED IBM/AIX
C       GETENV SUBPROGRAM CALL TO INTEL/LINUX SYNTAX
C 2013-01-13  J. WHITING  FINAL PORT TO WCOSS -- UPDATED DOC BLOCKS;
C       REPLACED TESTS VS BMISS W/ IBFMS FUNCTION; REPLACED EXPLICIT
C       ASSIGNMENT OF BMISS W/ GETBMISS() FUNCTION.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS INPUT FILE
C                NAME, SECOND RECORD CONTAINS OUTPUT FILE NAME,
C                OPTIONAL THIRD RECORD CONTAINS TIME-WINDOWING
C                SPECIFICATIONS (THE YYYYMMDDHH<.HH> DATE OF THE
C                EARLIEST TIME TO DUMP AND THE YYYYMMDDHH<.HH> DATE OF
C                THE LATEST TIME TO DUMP), OPTIONAL FOURTH RECORD
C                CONTAINS DUP-CHECKING TOLERANCE LIMITS (IF FOURTH
C                RECORD IS MISSING, DEFAULT DUP-CHECKING TOLERANCE
C                LIMITS ARE USED, IF THIRD RECORD IS ALSO MISSING, NO
C                TIME WINDOWING IS PERFORMED)
C     UNIT 20  - UNCHECKED AND UNWINDOWED BUFR DUMP FILE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 50  - DUPLICATE CHECKED AND TIME WINDOWED BUFR DUMP FILE
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE ERREXIT
C       W3EMC    - ORDERS 
C       BUFRLIB  - DATELEN OPENBF COPYMG UFBTAB OPENMB COPYSB READMG
C                  IREADMG CLOSMG CLOSBF MESGBC IBFMS GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL TERMINATION
C
C REMARKS:  NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      PROGRAM BUFR_DUPSST
 
      PARAMETER (MXTS=8)
 
      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:)

      CHARACTER*500 FILI,FILO
      CHARACTER*80  TSTR,TSTR1
      CHARACTER*45  TEXT
      CHARACTER*8   SUBSET
      CHARACTER*3   DUMMY_MSGS

      DIMENSION    NDUP(0:5)

      REAL(8)      ADATE,BDATE,CDATE,DDATE,RDATE,UFBTAB_8

      LOGICAL      DUPES,SSTRET
 
      DATA TSTR  /'CLAT CLON SST1 SSTYPE DAYS HOUR MINU SSTSRC'/
      DATA TSTR1 /'CLAT CLON SST1 SSTYPE DAYS HOUR MINU SAID  '/

      DATA ADATE /00000000.00_8/
      DATA BDATE /99999999.00_8/
      DATA DEXY  /0/
      DATA DSST  /0/
      DATA DTYP  /0/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPSST',2013,0013,0062,'NP22')

      print *
      print * ,'---> Welcome to BUFR_DUPSST - Version 01-13-2013'
      print *

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

      BMISS = GETBMISS()     ! assign default value for "missing"

C  SET THE COUNTERS TO INITIAL VALUES
C  ----------------------------------

      ISUB  =  0
      NDUP  =  0
      IREC  =  1
      LUBFI = 20
      LUBFJ = 50

C  READ I/O FILENAMES AND ANY OVERRIDE VALUES FOR THINNING PARAMETERS
C  ------------------------------------------------------------------
C     DEFAULT PARAMETERS:
C     ADATE = 00000000.00  LOWER LIMIT FOR DATE/TIME
C     BDATE = 99999999.00  UPPER LIMIT FOR DATE/TIME
C     DEXY = 0.0  TOLERANCE FOR LAT,LON CHECKS
C     DSST = 0.0  TOLERANCE FOR SST CHECK
C     DTYP = 0.0  TOLERANCE FOR SST TYPE CHECK (ALL TYPES EXCEPT
C                 PHYSICAL SST RETRIEVALS IN NC012017 AND NC012018)
C     DTYP = 0.0  TOLERANCE FOR SATELLITE ID CHECK (PHYSICAL SST
C                 RETRIEVALS IN NC012017 AND NC012018)
C  --------------------------------------------------------------------
 
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILI,FILI(1:NBYTES_FILI)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILO,FILO(1:NBYTES_FILO)

cppppp
ccc   print *, 'file fili is ',nbytes_fili,' bytes long'
ccc   print *, 'file filo is ',nbytes_filo,' bytes long'
cppppp

      READ(5,*,END=1) ADATE,BDATE
      READ(5,*,END=1) DEXY,DSST,DTYP

    1 CONTINUE

C  OPEN FILE TEMPORARILY TO SEE WHAT THE BUFR MESSAGE TYPE IS (SUBSET)
C  -------------------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')

      CALL OPENBF(LUBFI,'IN',LUBFI)
      IF(IREADMG(LUBFI,SUBSET,IDATE).NE.0) THEN
         PRINT *, '#####BUFR_DUPSST - NO DATA IN INPUT FILE - STOP'
         CALL W3TAGE('BUFR_DUPSST')
         CALL ERREXIT(00)
      ENDIF
      CALL CLOSBF(LUBFI)

      SSTRET=(SUBSET.EQ.'NC012017' .OR. SUBSET.EQ.'NC012018')

      IF(SSTRET)  THEN  ! Physical SST retrievals come here
         TEXT = 'TOLERANCE FOR SATELLITE ID CHECK ........... '
      ELSE              ! All other SST types come here
         TEXT = 'TOLERANCE FOR SST TYPE CHECK ............... '
      ENDIF

      IF(BDATE.NE.99999999.00_8) THEN
         PRINT 200, ADATE,BDATE
      ELSE
         PRINT 201
      ENDIF
  200 FORMAT(/'REQUESTED EARLIEST DATE IS ....... ',F15.2/
     .        'REQUESTED LATEST   DATE IS ....... ',F15.2)
  201 FORMAT(/'@@@@ AS REQUESTED, NO TIME WINDOW TRIMMING IS PERFORMED'/
     .        '@@@@ ALL NON-DUPLICATES ARE RETAINED REGARDLESS OF TIME')
      PRINT 202, FILI(1:NBYTES_FILI),FILO(1:NBYTES_FILO),DEXY,DSST,TEXT,
     . DTYP
  202 FORMAT(/'UNCHECKED INPUT FILE IS         '/5X,A/
     .        'DUPLICATE CHECKED OUTPUT FILE IS'/5X,A//
     .        'BUFR_DUPSST PARAMETERS:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F5.1/
     .        3X,'TOLERANCE FOR SST CHECK (IN DEGREES K) ..... ',F5.1/
     .        3X,A45,F5.1)

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')

      CALL MESGBC(LUBFI,MSGT,ICOMP)
      IF(ICOMP.EQ.1) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   C O M P R E S S E D"/'//
     .    '"FIRST MESSAGE TYPE FOUND IS",I5/)', MSGT
      ELSE  IF(ICOMP.EQ.0) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   '//
     .    'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .    MSGT
      ELSE IF(ICOMP.EQ.-1)  THEN
         PRINT'(//"ERROR READING INPUT BUFR FILE - MESSAGE '//
     .    'COMPRESSION UNKNOWN"/)'
      ELSE  IF(ICOMP.EQ.-3)  THEN
         PRINT'(/"INPUT BUFR FILE DOES NOT EXIST"/)'
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"INPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF

      CALL CLOSBF(LUBFI)

C  COUNT THE NUMBER OF SUBSETS IN THE FILE TO ALLOCATE SPACE
C  ---------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,MXTB,' ')

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8 = BMISS
      JDUP  = 0
      IORD  = 0

C  MAKE A TABLE OUT OF THE LATS, LONS, SST, SST-TYPE, TIME COORDINATES
C   AND SST-OBS SOURCE
C  -------------------------------------------------------------------
 
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      IF(SSTRET) THEN ! Physical SST retrievals come here
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTR1)
      ELSE            ! All other SST types come here
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTR)
      ENDIF
 
C  GET A SORTED INDEX OF THE REPORTS BY:
C   SATELLITE ID, SST-TYPE, SST, AND LAT/LON FOR SST RETRIEVALS
C   SST-OBS SOURCE, SST-TYPE, SST, AND LAT/LON FRO ALL OTHER SST TYPES
C  -------------------------------------------------------------------
 
      CALL ORDERS( 2,IWORK,TAB_8(8,1),IORD,NTAB,MXTS,8,2) ! sat.id(retr)
                                                          ! source(rest)
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! sst-type
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! sst
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! lon
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! lat
 
C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES
C  ---------------------------------------------------

      DO K=1,NTAB-1
         IREC = IORD(K)
         JREC = IORD(K+1)
         IF(
     .    NINT(ABS(TAB_8(1,IREC)-TAB_8(1,JREC))*100.).LE.NINT(DEXY*100.)
     .    .AND.
     .    NINT(ABS(TAB_8(2,IREC)-TAB_8(2,JREC))*100.).LE.NINT(DEXY*100.)
     .    .AND.
     .    NINT(ABS(TAB_8(3,IREC)-TAB_8(3,JREC))*100.).LE.NINT(DSST*100.)
     .    .AND.
     .    NINT(ABS(TAB_8(4,IREC)-TAB_8(4,JREC))*100.).LE.NINT(DTYP*100.)
     .    .AND.
     .   NINT(ABS(TAB_8(8,IREC)-TAB_8(8,JREC))*100.).LE.NINT(DTYP*100.))
     .    JDUP(IREC) = 1
      ENDDO
 
C  TRIM THE EXCESS DATA FROM THE EXACT TIME WINDOW (IF REQUESTED)
C  --------------------------------------------------------------

      IF(BDATE.NE.99999999.00_8) THEN
         CDATE = MOD(ADATE,10000._8)
         DDATE = MOD(BDATE,10000._8)
         DO K=1,NTAB
            RDATE = TAB_8(5,K)*1E2 + TAB_8(6,K) + TAB_8(7,K)/60.
            IF(CDATE.LE.DDATE) THEN
               IF(RDATE.LT.CDATE .OR.  RDATE.GT.DDATE) JDUP(K) = 2
            ELSE
               IF(RDATE.LT.CDATE .AND. RDATE.GT.DDATE) JDUP(K) = 2
            ENDIF
         ENDDO
      ENDIF
 
C  WRITE A DUP-CHECKED AND TIME-WINDOWED FILE
C  ------------------------------------------
 
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL OPENBF(LUBFI,'IN ',LUBFI)
      CALL OPENBF(LUBFJ,'OUT',LUBFI)
 
      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

C  If input file doesn't contain dummy center and dump time messages 1
C   and 2 (after table messages), before doing anything call closmg
C   with a negative unit number to signal routine that it should not
C   write out ANY messages with zero subsets in them - this holds for
C   all subsequent calls to closmg in this routine, even those done
C   through other bufrlib routines (and even for those calls where the
C   sign of the unit number is positive)
C  --------------------------------------------------------------------

      IF(DUMMY_MSGS.NE.'YES') CALL CLOSMG(-LUBFJ)

      DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
         NSUBS = NMSUB(LUBFI)

C  If no subsets in msg & dummy msgs not expected loop to next input msg
C  ---------------------------------------------------------------------

         IF(NSUBS.LE.0.AND.DUMMY_MSGS.NE.'YES')  CYCLE

         DUPES = .FALSE.
         IF(NSUBS.GT.0)  THEN
            DO N=1,NSUBS
               if(isub+n.gt.ntab)  then
                  idup = 5
               else
                  IDUP = JDUP(ISUB+N)
               endif
               IF(IDUP.GT.0) DUPES = .TRUE.
            ENDDO
         ENDIF
         IF(DUPES) THEN
            CALL OPENMB(LUBFJ,SUBSET,IDATE)
            DO WHILE(IFBGET(LUBFI).EQ.0)
               ISUB = ISUB+1
               if(isub.gt.ntab)  then
                  idup = 5
               else
                  IDUP = JDUP(ISUB)
               endif
               IF(IDUP.EQ.0) THEN
                  CALL COPYSB(LUBFI,LUBFJ,IRET)
                  NDUP(IDUP) = NDUP(IDUP)+1
               ELSE
                  CALL COPYSB(LUBFI,00000,IRET)
                  NDUP(IDUP) = NDUP(IDUP)+1
               ENDIF
            ENDDO
         ELSE
            IF(NSUBS.GT.0) THEN
               DO N=1,NSUBS
                  IDUP = JDUP(ISUB+N)
                  NDUP(IDUP) = NDUP(IDUP)+1
               ENDDO
            ENDIF

C  In the event that the input file contains dummy center and dump time
C    messages 1 and 2 (after table messages), call closmg with a
C    positive unit number to signal routine that it should write out
C    these messages even though they have zero subsets in them
C  If the input file does not contain dummy messages, a positive unit
C    number here is immaterial because closmg was already called with
C    a negative unit number immediately after the output file was
C    opened (and this holds for all subsequent calls to closmg
C    regardless of the sign of the unit number)
C  -------------------------------------------------------------------

            CALL CLOSMG(LUBFJ)
            CALL COPYMG(LUBFI,LUBFJ)
            ISUB = ISUB+NSUBS
         ENDIF
      ENDDO
 
      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFJ)
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL MESGBC(LUBFJ,MSGT,ICOMP)
      IF(ICOMP.EQ.1) THEN
         PRINT'(/"OUTPUT BUFR FILE MESSAGES   C O M P R E S S E D"/'//
     .    '"FIRST MESSAGE TYPE FOUND IS",I5/)', MSGT
      ELSE  IF(ICOMP.EQ.0) THEN
         PRINT'(/"OUTPUT BUFR FILE MESSAGES   '//
     .    'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .    MSGT
      ELSE IF(ICOMP.EQ.-1)  THEN
         PRINT'(//"ERROR READING OUTPUT BUFR FILE - MESSAGE '//
     .    'COMPRESSION UNKNOWN"/)'
      ELSE  IF(ICOMP.EQ.-3)  THEN
         PRINT'(/"OUTPUT BUFR FILE DOES NOT EXIST"/)'
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"OUTPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF
      CLOSE(LUBFJ)
 
C  GENERATE REPORT
C  ---------------
 
      PRINT 300, ISUB,NDUP(5),MXTB,NTAB,NDUP(0),NDUP(1),NDUP(2)
  300 FORMAT(/'BUFR_DUPSST READ IN A TOTAL OF',I12,' REPORTS'/
     .        ' A TOTAL OF ',I12,' REPORTS WERE SKIPPED DUE TO BEING ',
     .        'OVER THE LIMIT OF ',I12/
     .        'BUFR_DUPSST CHECKED A TOTAL OF',I12,' REPORTS'//
     .        'NUMBER OF REPORTS WRITTEN OUT ..................',I9/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK .....................',I9/
     .        '   BEING OUTSIDE TIME WINDOW FOR TRIMMING ......',I9/)

C  END OF PROGRAM
C  --------------

      CALL W3TAGE('BUFR_DUPSST')
      STOP

C  ERROR EXITS
C  -----------

  900 CONTINUE

      PRINT *, '#####BUFR_DUPSST - EOF/ERR READING STDIN'
      CALL W3TAGE('BUFR_DUPSST')
      CALL ERREXIT(99)

  901 CONTINUE

      PRINT *, '#####BUFR_DUPSST - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPSST')
      CALL ERREXIT(99)

      END

