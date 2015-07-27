C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPAIR
C   PRGMMR: MELCHIOR/KEYSER  ORG: NP22        DATE: 2015-06-16
C
C ABSTRACT: PROCESSES ANY COMBINATION UP TO SEVEN DUMP FILES
C   CONTAINING TYPES THAT ULTIMATELY GO INTO EITHER THE "AIRCFT" DUMP
C   {BUFR MESSAGE TYPE 004, SUBTYPES 001 (AIREP FORMAT), 002 (PIREP
C   FORMAT), 003 (AMDAR FORMAT), 006 (EUROPEAN AMDAR, BUFR FORMAT) AND
C   009) (CANADIAN AMDAR, BUFR FORMAT)} OR INTO THE "AIRCAR" DUMP {BUFR
C   MESSAGE TYPE 004, SUBTYPES 004 (MDCRS FROM ARINC) AND 007 (MDRCS
C   FROM AFWA)}.  PERFORMS A SINGLE DUP-CHECK FOR REPORTS ACROSS ALL
C   APPLICABLE FILES GOING INTO EITHER THE "AIRCFT" DUMP OR INTO THE
C   "AIRCAR" DUMP.  DOES NOT INCLUDE TAMDAR IN SUBTYPES 008, 010, 012
C   AND 013 WHICH GO INTO "AIRCFT" DUMP.  INFORMATION IS READ
C   SEPARATELY FROM EACH FILE THAT IS PRESENT, AND IS THEN COMBINED
C   INTO TABLES USED FOR THE DUP-CHECK.  THE ALGORITHM SORTS THE
C   REPORTS IN ASCENDING ORDER OF LAT, LON, OBS TIME (DAY DOWN TO
C   SECOND), HEIGHT, AND RECEIPT TIME (YEAR DOWN TO MINUTE).  IN THE
C   DUPLICATE CHECKING LOGIC, ADJACENT, SORTED REPORT PAIRS ARE CHECKED
C   FOR LAT, LON, OBS TIME (TO THE SECOND) AND FLIGHT LEVEL, ALL BASED
C   ON TOLERANCE LIMITS WHICH CAN VARY BASED ON THE TYPE OF REPORT
C   (E.G., AIREP, PIREP, AMDAR, MDCRS), WHETHER OR NOT THE REPORT IS
C   RE-ENCODED FROM AN ORIGINAL AIREP OR AMDAR TYPE INTO AN AIREP TYPE
C   BY AFWA (OR PREVIOUSLY CARSWELL OR TINKER), AND WHETHER THIS IS A
C   REAL-TIME OR HISTORICAL RUN.  THE REPORT USUALLY SELECTED IS THE
C   BULLETIN LAST RECEIVED, HOWEVER IF ONE REPORT IN THE PAIR IS FROM
C   AFWA AND THE OTHER IS NOT, THE NON-AFWA REPORT IS ALWAYS SELECTED.
C   THE WORKING FILE NAMES OF THE INPUT DUMP FILES (IN EITHER THE "NEW"
C   FORM x_ttt.sss, WHERE ttt IS BUFR TYPE, sss IS BUFR SUBTYPE, AND x
C   IS AN ORDERING INDEX; OR THE "OLD" FORM ttt.sss) ARE READ FROM
C   STANDARD INPUT (UNIT 5) AT THE START OF THIS PROGRAM. THE OUTPUT
C   DUP-CHECKED FILES WILL BE WRITTEN TO THE SAME FILE NAMES.  ALL
C   OTHER FILE CONNECTIONS ARE MADE THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 1997-01-22  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C     8 BYTE STORAGE
C 2002-03-05  D. KEYSER   IMPROVED DOCUMENTATION; IMPROVED STANDARD
C     OUTPUT PRINT; ADDED CALL TO COMPRESS_CHECK TO INDICATE IF INPUT/
C     OUTPUT FILES ARE COMPRESSED OR UNCOMPRESSED; ACCOUNTS FOR CHANGE
C     FROM MNEMONIC "ICLI" TO "BORG" {FOR BULLETIN BEING MONITORED
C     "CCCC" (BULLETIN LOCATION IDENTIFIER)} IN INPUT 004.001, 004.002
C     AND 004.003 WORKING DUMP FILES AFTER 5/2002 (WILL STILL WORK
C     PROPERLY FOR DUMP FILES PRIOR TO 5/2002)
C 2002-11-14  B. FACEY    INCREASED MXTB FROM 50000 TO 60000 TO
C     HANDLE LARGER VOLUMES OF AIRCRAFT DATA
C 2002-11-14  D. KEYSER   ADDED INLINE VERSION OF BUFRLIB ROUTINE
C     UFBTAB WHICH WILL NOT ABORT WHEN THERE ARE TOO MANY REPORTS
C     (> MXTB) INPUT - RATHER IT WILL JUST PROCESS MXTB REPORTS BUT
C     PRINT A DIAGNOSTIC (NOTE: THIS IS A TEMPORARY CHANGE UNTIL THE
C     NEXT VERSION OF THE BUFRLIB IS IMPLEMENTED WITH THE UPDATED
C     UFBTAB); OTHER MODIFICATIONS TO MAIN PROGRAM TO PREVENT ARRAY
C     OVERFLOW WHEN THERE ARE > MXTB REPORTS; IMPROVED DIAGNOSTIC
C     PRINT, ESP. WHEN > MXTB REPORTS; ADDED STATUS FILE IN UNIT 60
C     THAT IS WRITTEN TO ONLY WHEN THIS PROGRAM COMPLETES SUCCESSFULLY
C     (TRANSF. TO DUMPJB SCRIPT)
C 2003-09-02  D. KEYSER   ADDED FLEXIBILITY IN READING AND CHECKING
C     INPUT FILE NAMES; ADDED A FOURTH DUMP FILE TO BE CHECKED,
C     CONTAINING E-AMDAR AIRCRAFT (004.006) WHICH HAS HIGH-ACCURACY
C     LAT/LON (CLATH/CLONH); REMOVED INLINE VERSION OF UFBTAB (CHANGES
C     NOTED IN PREVIOUS IMPLEMENTATION ARE NOW IN BUFRLIB VERSION);
C     REPLACED CALL TO IN-LINE SUBROUTINE COMPRESS_CHECK WITH CALL TO
C     NEW BUFRLIB ROUTINE MESGBC
C 2003-12-15  D. KEYSER   NO LONGER TESTS A WHAT FILE A REPORT IS IN TO
C     SEE IF IT SHOULD KEY ON HI-ACCURACY LAT/LON (CLATH/CLONH) IN
C     DUPLICATE CHECKS, RATHER JUST CHECKS IF FIRST SUBSET HAS MISSING
C     LOW-ACCURACY LAT (CLAT) - IF SO ASSUMES ALL REPORTS ENCODE CLATH/
C     CLONH - GENERALIZES THIS CHECK AND ALLOWS FOR REPORT TYPES WHICH
C     MAY CHANGE FROM CLAT/CLON TO CLATH/CLONH AT SOME POINT IN TIME
C     (AND WILL STILL WORK FOR HISTORICAL RUNS PRIOR TO CHANGEOVER)
C 2006-03-02  D. KEYSER   CHECKS TO SEE IF INPUT BUFR FILE(S) CONTAIN
C     "DUMMY" MESSAGES CONTAINING DUMP CENTER TIME AND PROCESSING TIME,
C     RESP. IN FIRST TWO MESSAGES OF INPUT FILE (AFTER TABLE MSGS) BY
C     CHECKING THE VALUE OF DUMPJB SCRIPT VARIABLE "DUMMY_MSGS" (READ
C     IN VIA "GETENV") - IF NOT WILL NOT PROCESS INPUT BUFR MESSAGES
C     WITH ZERO SUBSETS AND WILL CALL BUFRLIB ROUTINE CLOSMG WITH A
C     NEGATIVE UNIT NUMBER ARGUMENT PRIOR TO ALL PROCESSING IN ORDER TO
C     SIGNAL IT THAT ANY OUTPUT BUFR MESSAGES WITH ZERO SUBSETS SHOULD
C     BE SKIPPED (NOT WRITTEN OUT) - CODE HAD BEEN HARDWIRED TO ALWAYS
C     ASSUME DUMMY MESSAGES WERE IN THE INPUT FILE; MODIFIED TO HANDLE
C     PROCESSING OF CANADIAN AMDAR REPORTS WITH MESSAGE TYPE NC004009
C 2007-03-23  D. KEYSER   INTRODUCED ALLOCATABLE ARRAYS TO AVOID ARRAY
C     OVERFLOW PROBLEMS, DETERMINES SIZE OF ARRAYS BY CALLING UFBTAB
C     WITH NEGATIVE UNIT NUMBER TO SIMPLY COUNT SUBSETS
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- ADAPTED IBM/AIX
C     GETENV SUBPROGRAM CALL TO INTEL/LINUX SYNTAX
C 2013-01-13  J. WHITING  FINAL WCOSS PORT - UPDATED DOC BLOCKS; 
C     REPLACED TESTS VS BMISS WITH IBFMS FUNCTION; REPLACED EXPLICIT
C     ASSIGNMENT OF BMISS W/ GETBMISS() FUNCTION
C 2013-08-22  D. KEYSER   EXITS GRACEFULLY IF A TOTAL OF ZERO INPUT
C     SUBSETS (REPORTS) ARE READ IN (BEFORE COULD SEG FAULT)
C 2014-11-07  D. KEYSER   DECLARE FUNCTION GETBMISS AND ITS RETURN
C     VALUE BMISS AS REAL*8 TO GET A RELIABLE VALUE FOR BMISS IN PRINT
C     STATEMENTS
C 2015-06-16  S. Melchior
C      - Now duplicate checks MDCRS reports (004.004 or 004.007) which
C        will later be part of the "aircar" dump (duplcate check moved
C        from upstream "dupcor" code which did not consider height or
C        obs time down to second as this code does in its duplicate
C        checking).
C      - Tolerances tightened to an exact match when duplicate checking
C        MDCRS or AMDAR report against like-type for obs date after
C        January 1, 2010 when it is known that AFWA was no longer re-
C        encoding MDCRS or AMDAR report as AIREPs.
C           - Tolerances remain as before (loosened) if 1 of 2 dups
C             being checked is an AIREP or PIREP or obs date is prior
C             to January 1, 2010 for any type (and remain even more
C             loosened as before if 1 of 2 AIREPs or PIREPs is from
C             AFWA).
C      Changes above prevent near-duplcate MDCRS or AMDAR reports from
C      being tossed as before. This can result in thousands of reports
C      that were previously tossed now being retained.  Historic runs
C      (pre-2010) are not changed since re-encoded AFWA reports
C      appeared then. Duplcate AIREP and PIREP reports can still come
C      in from different sources with slightly different lat/lon, time
C      or elevation, thus old tolerances are kept for these.
C 2015-06-16  D. Keyser
C      - Additional changes related to above:
C         - Receipt time is now stored so that latest arriving report
C           is selected when there are duplicates (as with "dupcor" and
C           other dup-check codes). Especially needed now that MDCRS
C           are duplicate checked here rather than in upstream
C           "dupcor" code.
C         - Added checks to prevent waste of time checking reports
C           already flagged as dups against subsequent reports.  Does
C           not change answers but improves efficiency.
C         - Added requirement to retain AMDAR report when it duplicates
C           an AIREP or PIREP (thus tossing AIREP or PIREP), or to
C           retain an AIREP report when it duplicates a PIREP (thus
C           tossing PIREP).  Ensures best quality report is retained.
C         - If reading a MCDRS file (004.004 or 004.007) which will
C           later be part of the "aircar" dump and obs date is prior to
C           January 1, 2010, do not do any processing here (duplicates
C           are still removed by upstream "dupcor" code for obs date
C           prior to January 1, 2010).
C         - Added diagnostic print for debugging.  This is commented
C           out.
C         - Added a more detailed report summary print at the end.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - RECORDS CONTAINING THE WORKING INPUT
C                FILE NAMES FOR ALL AIRCRAFT TYPES EVENTUALLY BEING
C                COMBINED INTO A SINGLE DUMP FILE (for either "aircft"
C                or "aircar" dumps) - THE ONLY FILE NAMES CONSIDERED
C                BY THIS PROGRAM ARE *004.001 (AIREP FORMAT), *004.002
C                (PIREP FORMAT), *004.003 (AMDAR FORMAT), *004.006
C                (E-AMDAR, BUFR FORMAT) and *004.009 (CANADIAN AMDAR,
C                BUFR FORMAT) (all included in the "aircft" dump), or
C                *004.004 (MDCRS from ARINC) and *004.007 (MDCRS from
C                AFWA) (both included in the "aircar" dump) - OTHER
C                FILES MAY BE INCLUDED HERE, BUT THEY WILL NOT BE
C                MODIFIED BY THIS PROGRAM; THE OUTPUT FILE NAMES WILL
C                BE THE SAME AS THE INPUT NAMES HERE.
C     UNIT 20  - UNCHECKED BUFR FILE(S)
C
C   OUTPUT FILES:
C     UNIT 20  - DUPLICATE CHECKED BUFR FILE(S)
C     UNIT 50  - WORKSPACE (SCRATCH) FILE(S)
C     UNIT 60  - STATUS FILE WHOSE PRESENCE INDICATES THIS PROGRAM
C                COMPLETED SUCCESSFULLY
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE ERREXIT IW3JDN
C       W3EMC    - ORDERS
C       BUFRLIB  - DATELEN OPENBF COPYMG UFBTAB OPENMB COPYBF STATUS
C                  COPYSB  CLOSMG CLOSBF NEMTAB MESGBC IBFMS  GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL RUN
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      PROGRAM BUFR_DUPAIR
 
      PARAMETER (MXTS=15)
      PARAMETER (NFILES=7)  ! Number of input files being considered
 
      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      real(8),allocatable :: rab_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:)

      REAL(8) UFBTAB_8,TAB7_8,BMISS,GETBMISS,tab1_i_8,tab1_j_8
      CHARACTER*80 TSTR,TSTRH,TSTR_N,TSTR_O,TSTRH_N,TSTRH_O,
     $ FILI(NFILES),FILO,FILE,rstr,tstr_n_mdcrs,tstr_o_mdcrs
      CHARACTER*8  SUBSET,CTAB7,ctab1_i,ctab1_j
      character*19 ctext_file(nfiles)
      CHARACTER*3  DUMMY_MSGS
      CHARACTER*1  CDUMMY

      DIMENSION    IMST(9),ntab_file(nfiles)
      DIMENSION    NDUP(0:4),IPTR(2,NFILES),ndup_file(0:4,nfiles)

      LOGICAL      DUPES,PIREP,AIRCFT,mdcrs

      EQUIVALENCE  (TAB7_8,CTAB7)
      equivalence  (tab1_i_8,ctab1_i),(tab1_j_8,ctab1_j)

      DATA TSTR_N
     $ /'RPID CLAT  CLON  DAYS HOUR MINU BORG PSAL FLVL HEIT HMSL
     $ IALT SECO YEAR MNTH '/
      DATA TSTRH_N
     $ /'ACRN CLATH CLONH DAYS HOUR MINU BORG PSAL FLVL HEIT HMSL
     $ IALT SECO YEAR MNTH '/
      DATA TSTR_O
     $ /'RPID CLAT  CLON  DAYS HOUR MINU ICLI PSAL FLVL HEIT HMSL
     $ IALT SECO YEAR MNTH '/
      DATA TSTRH_O
     $ /'ACRN CLATH CLONH DAYS HOUR MINU ICLI PSAL FLVL HEIT HMSL
     $ IALT SECO YEAR MNTH '/
      data tstr_n_mdcrs
     $ /'ACRN CLAT  CLON  DAYS HOUR MINU BORG PSAL FLVL HEIT HMSL
     $ IALT SECO YEAR MNTH '/
      data tstr_o_mdcrs
     $ /'ACRN CLAT  CLON  DAYS HOUR MINU ICLI PSAL FLVL HEIT HMSL
     $ IALT SECO YEAR MNTH '/
      data rstr  /'RCYR  RCMO  RCDY RCHR RCMI                     '/

      data ctext_file/
     $           'AIREP format       ',
     $           'PIREP format       ',
     $           'AMDAR format       ',
     $           'MDCRS-ARINC/BUFR   ',
     $           'EUROPEAN AMDAR/BUFR',
     $           'MDCRS-AWFA/BUFR    ',
     $           'CANADIAN AMDAR/BUFR'/

C  Tolerance parameters for reports with obs time after January 01,
C   2010 -and- the pair being checked contains some combination of
C   MDCRS 004004 or 004007) or AMDAR (004003, 004006, 004009) reports:
C  -------------------------------------------------------------------

      data dexyn   / 0.0/ ! lat/lon
      data drhrn   / 0.0/ ! hour
      data delvn   / 0.0/ ! height

C  Tolerance parameters for reports with obs time prior to January 01,
C   2010 -or- at least one of the pair being checked contains an AIREP
C   (004001) or PIREP (004002) report:
C  -------------------------------------------------------------------

      data dexyo   / 0.05/! lat/lon if neither report in pair is AFWA
      data dexyoa  / 1.50/! lat/lon if 1 or both reports in pair is AFWA
                          !  (note: DEXYOa must be .ge. DEXYO)
      data drhro   / 0.2/ ! hour
      data delvo   /10.0/ ! height

C  Tolerance parameters for all reports:
C  -------------------------------------

      DATA DDAY    / 0.0/ ! day


      DATA IMST  /   1,   2,   3,   4, -99,   5,   6, -99,   7/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPAIR',2015,0167,0054,'NP22')

      print *
      print * ,'---> Welcome to BUFR_DUPAIR - Version 06-16-2015'
      print *

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib
 
C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

      BMISS = GETBMISS()     ! assign default value for "missing"
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *

C  SET THE COUNTERS TO INITIAL VALUES
C  ----------------------------------

      LUBFI = 20
      LUBFJ = 50

C  STORE THE FILENAMES TO PROCESS FROM STANDARD INPUT
C  --------------------------------------------------
 
      FILI(1:NFILES)(1:4) = 'NONE'

      AIRCFT = .FALSE.
      mdcrs = .false.
 
1     CONTINUE

      READ(5,'(A80)',END=2) FILE
      DO  I=1,10
         IF(FILE(I:I+3).EQ.'004.') THEN
            READ(FILE(I+4:I+6),'(I3)') MST
            IF(MST.LE.4.OR.MST.EQ.6.or.mst.eq.7.OR.MST.EQ.9) THEN
               FILI(IMST(MST)) = FILE
               AIRCFT = .TRUE.
               PRINT *, ' >> WILL CHECK ',FILE(I:I+6)
               if(file(i:i+6).eq.'004.004'.or.file(i:i+6).eq.'004.007') 
     .          mdcrs = .true.
            ENDIF
            EXIT
         ENDIF
      ENDDO
      GOTO 1

2     CONTINUE
      IF(.NOT.AIRCFT) THEN
         PRINT *
         PRINT *,'BUFR_DUPAIR: NO AIRCRAFT TO CHECK'
         PRINT *
         CALL W3TAGE('BUFR_DUPAIR')
         STOP
      ELSE
         print 200, dday,dexyn,drhrn,delvn,dexyoa,dexyo,drhro,delvo
  200 FORMAT(/'BUFR_DUPAIR PARAMETERS:'/
     .        'For all reports:'/
     .        3X,'TOLERANCE FOR DAY CHECK (IN DAYS) .......... ',F7.3/
     .        'For reports with obs time after January 01, 2010 -and-'/
     .        ' the pair being checked contains some combination of ',
     .        'MDCRS or AMDAR reports:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F7.3/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F7.3/
     .        3X,'TOLERANCE FOR HEIGHT CHECK (IN METERS) ..... ',F7.3/
     .        'For reports with obs time prior to January 01, 2010 ',
     .        '-or-'/' one of the pair being checked contains an AIREP',
     .        ' or PIREP report:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) IF'/
     .        5X,'ONE OR BOTH REPORTS IN PAIR IS FROM AFWA . ',F7.3/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) IF'/
     .        5X,'NEITHER REPORT IN PAIR IS FROM AFWA ...... ',F7.3/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F7.3/
     .        3X,'TOLERANCE FOR HEIGHT CHECK (IN METERS) ..... ',F7.3/)
      ENDIF
 
C  COUNT THE NUMBER OF SUBSETS AMONGST ALL FILES TO ALLOCATE SPACE
C  ---------------------------------------------------------------

      MXTB = 0
      DO I=1,NFILES
         IF(FILI(I)(1:4).EQ.'NONE') CYCLE
         CALL CLOSBF(LUBFI)
         OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
         CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if
                                  ! an embedded BUFR table is read
         CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,NUM_SUBSETS,' ')
         CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of prnt
         MXTB = MXTB + NUM_SUBSETS
      ENDDO

      ISUB = 0
      NTAB = 0

      IF(MXTB.EQ.0) THEN
         PRINT *
         PRINt *, '### WARNING: A total of ZERO input aircraft reports'
         PRINT *
         GO TO 400
      ENDIF

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      allocate(rab_8(mxts,mxtb),stat=i);if(i.ne.0) goto 901
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8 = BMISS
      rab_8 = bmiss

C  MAKE A TABLE OUT OF THE LATS, LONS, HEIGHT, OBS TIME
C   COORDINATES AND RECEIPT TIME COORDINATES
C  ----------------------------------------------------
 
      IPTR = 0
      IPT  = 1

      DO I=1,NFILES
         IF(FILI(I)(1:4).NE.'NONE') THEN
            CALL CLOSBF(LUBFI)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')

            CALL MESGBC(LUBFI,MSGT,ICOMP)
            IF(ICOMP.EQ.1) THEN
               PRINT'(/"INPUT BUFR FILE",I2," MESSAGES   '//
     .          'C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",'//
     .          'I5/)', I,MSGT
               PRINT'("#####BUFR_DUPAIR (UFBTAB) CANNOT PROCESS '//
     .            'COMPRESSED BUFR MESSAGES -- FATAL ERROR")'
               CALL W3TAGE('BUFR_DUPAIR')
               CALL ERREXIT(99)
            ELSE  IF(ICOMP.EQ.0) THEN
               PRINT'(/"INPUT BUFR FILE",I2," MESSAGES   '//
     .          'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND '//
     .          'IS",I5/)', I,MSGT
            ELSE IF(ICOMP.EQ.-1) THEN
               PRINT'(//"ERROR READING INPUT BUFR FILE",I2," - '//
     .          'MESSAGE COMPRESSION UNKNOWN"/)', I
            ELSE  IF(ICOMP.EQ.-3) THEN
               PRINT'(/"INPUT BUFR FILE",I2," DOES NOT EXIST"/)', I
            ELSE  IF(ICOMP.EQ.-2) THEN
               PRINT'(/"INPUT BUFR FILE",I2," HAS NO DATA MESSAGES"/'//
     .          '"FIRST MESSAGE TYPE FOUND IS",I5/)', I,MSGT
            ENDIF

            CALL OPENBF(LUBFI,'IN',LUBFI)

C  Check to see if the new (post 5/2002) version of the mnemonic
C   table is being used here
C  -------------------------------------------------------------
      
            CALL STATUS(LUBFI,LUN,IDUMMY1,IDUMMY2)
            CALL NEMTAB(LUN,'BORG',IDUMMY1,CDUMMY,IRET) ! "BORG" is only
                                                        !  in new table
            CALL CLOSBF(LUBFI)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            IF(IRET.GT.0) THEN
               TSTR  = TSTR_N
               if(i.eq.4) tstr = tstr_n_mdcrs
               TSTRH = TSTRH_N
            ELSE
               TSTR  = TSTR_O
               if(i.eq.4) tstr = tstr_o_mdcrs
               TSTRH = TSTRH_O
            ENDIF

            CALL UFBTAB(LUBFI,TAB_8(1,IPT),MXTS,MXTB-IPT+1,NTAB,TSTR)
            IF(IBFMS(TAB_8(2,IPT)).EQ.1) THEN             ! data missing
               OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
              CALL UFBTAB(LUBFI,TAB_8(1,IPT),MXTS,MXTB-IPT+1,NTAB,TSTRH)
            ENDIF
            call ufbtab(lubfi,rab_8(1,ipt),mxts,mxtb-ipt+1,ntab,rstr)
            IPTR(1,I) = IPT
            IPTR(2,I) = IPT+NTAB-1
            IPT = IPT+NTAB
         ENDIF
      ENDDO

      NTAB = IPT-1

C  The arrays containing receipt time coordinates are set to negative
C   values so that the key on them in sort routine ORDERS below will
C   order them from latest actual receipt time to earliest - this is
C   needed because logic is designed to normally toss the second report
C   in a duplicate pair and we want to retain the one with the latest
C   receipt time.
C  --------------------------------------------------------------------

      rab_8 = -rab_8

C  INITIAL VALUES FOR MARKERS AND COUNTERS
C  ---------------------------------------
 
      JDUP = 0
      NDUP = 0
      ndup_file = 0
      ntab_file = 0
 
C  Check obs time (year, month and day) of first report stored in
C   memory and save its Julian day in jnddata
C  Store control Julian day for January 1, 2010 in jndcntrl
C  - these will be used later to determine the tolerances for dup-check
C    and whether or not MDCRS reports should be duplicate checked here
C  --------------------------------------------------------------------

      jnddata= iw3jdn(int(tab_8(14,1)),int(tab_8(15,1)),int(tab_8(4,1)))
      jndcntrl = iw3jdn(2010,1,1)

      if(mdcrs.and.jnddata.lt.jndcntrl) then
         print 203
  203 format(/'@@@@ MDCRS DATA PRIOR TO JAN 1 2010 ARE NOT PROCESSED ',
     . 'BY THIS PROGRAM'/'@@@@ THIS PROGRAM IS DONE - STOP')
         write(60,'("ALL DONE")')
         call w3tage('BUFR_DUPAIR')
         call errexit(00)
      endif

      DO N=1,NTAB

         TAB_8(3,N) = MOD(TAB_8(3,N)+360.,360._8)

         IF(IBFMS(TAB_8(6,N)).EQ.1) TAB_8(6,N) = 0    ! data missing
         if(ibfms(tab_8(13,n)).eq.1) tab_8(13,n) = 0  ! data missing
         TAB_8(5,N) = TAB_8(5,N)+((TAB_8(6,N)+(TAB_8(13,N)/60.))/60.)

C  Find the height and store back in what was "MINU" slot
C  ------------------------------------------------------

         IF(IBFMS(TAB_8(8,N)).EQ.0) THEN               ! data not missing
            TAB_8(6,N) = TAB_8(8,N) ! PSAL - Height for AMDAR format
         ELSE  IF(IBFMS(TAB_8(9,N)).EQ.0) THEN         ! data not missing
            TAB_8(6,N) = TAB_8(9,N) ! FLVL - Height for AIREP/PIREP fmt
         ELSE IF(IBFMS(TAB_8(12,N)).EQ.0) THEN         ! data not missing
            TAB_8(6,N) = TAB_8(12,N)! IALT - Height for MDCRS fmt
         ELSE  IF(IBFMS(TAB_8(10,N)).EQ.0) THEN        ! data not missing
            TAB_8(6,N) = TAB_8(10,N)! HEIT - 1st choice height for
                                    !        E-EDAS/CAN-AMDAR
                                    !        (currently missing)
         ELSE
            TAB_8(6,N) = TAB_8(11,N)! HMSL - 2nd choice height for
                                    !        E-EDAS/CAN-AMDAR
                                    !        (available) -- or --
                                    !        default if all are missing
         ENDIF

C  Store input file index back in what was "FLVL" slot
C   TAB_8(9,N) = 1 --> b004/xx001 (AIREP format)
C              = 2 --> b004/xx002 (PIREP format)
C              = 3 --> b004/xx003 (AMDAR format)
C              = 4 --> b004/xx004 (MDCRS-ARINC/BUFR)
C              = 5 --> b004/xx006 (EUROPEAN AMDAR/BUFR)
C              = 6 --> b004/xx007 (MDCRS-AWFA/BUFR)
C              = 7 --> b004/xx009 (CANADIAN AMDAR/BUFR)
C  ----------------------------------------------------

         DO I=1,NFILES
            IF(N.GE.IPTR(1,I) .AND. N.LE.IPTR(2,I)) THEN
               TAB_8(9,N) = I
               ntab_file(i) =  ntab_file(i) + 1
               EXIT
            ENDIF
         ENDDO

C  Store Carswell/Tinker/AFWA ind. back in what was "ICLI"/"BORG" slot
C   (from here on these will simply be referrred to as AFWA)
C  -------------------------------------------------------------------

         TAB7_8 = TAB_8(7,N)
         TAB_8(7,N) = 0.

C  If obs time of report (based on first report in memory) is earlier
C   than January 01, 2010, or if report is AIREP or PIREP at any time,
C   then discriminate on AFWA header
C  --------------------------------------------------------------------

C        print *,'jnddata=',int(tab_8(14,n)),int(tab_8(15,n)),
C    .            int(tab_8(4,n))
         if(jnddata.lt.jndcntrl
     .      .or.tab_8(9,n).eq.1.or.tab_8(9,n).eq.2) then
           IF(CTAB7.EQ.'KAWN'.OR.CTAB7(1:4).EQ.'PHWR') THEN
             TAB_8(7,N) = 1.  ! Carswell/Tinker/AFWA indicator
           ENDIF
         endif
      ENDDO ! end DO N=1,NTAB
 
C  GET A SORTED INDEX OF THE REPORTS KEYED IN THIS ORDER: LAT, LON,
C  OBS TIME, HEIGHT, RECEIPT TIME
C  ----------------------------------------------------------------
 
      call orders( 2,iwork,rab_8(5,1),iord,ntab,mxts,8,2) ! rcpt minute
      call orders(12,iwork,rab_8(4,1),iord,ntab,mxts,8,2) ! rcpt hour
      call orders(12,iwork,rab_8(3,1),iord,ntab,mxts,8,2) ! rcpt day
      call orders(12,iwork,rab_8(2,1),iord,ntab,mxts,8,2) ! rcpt month
      CALL ORDERS(12,IWORK,RAB_8(1,1),IORD,NTAB,MXTS,8,2) ! rcpt year
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! height
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! obs time
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! day
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! lon
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! lat
 
C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES
C  ---------------------------------------------------
 
      LOOP1: DO L=1,NTAB-1
         K = L+1
         I = IORD(L)
         J = IORD(L+1)
         tab1_i_8 = tab_8(1,i)
         tab1_j_8 = tab_8(1,j)
cpppppppppp
ccc      print *, 'New input report number (L) ',L,' found in ',
ccc  .    'record (I) ',I
cpppppppppp
         if(jdup(i).ne.0) then
cpppppppppp
ccc         print *, '---> this report in record (I) has already been ',
ccc  .       'flagged as a duplicate - cycle'
cpppppppppp
            cycle LOOP1
         endif
cpppppppppp
ccc      print *, 'Next input report number (L+1) ',L+1,' found ',
ccc  .   'in record (J) ',J
cpppppppppp
         dexy = dexyn

C  If obs time of reports (based on first report in memory) is earlier
C   than January 01, 2010, or if one of the pair being checked contains
C   an AIREP or PIREP report, use the looser tolerances
C  --------------------------------------------------------------------

         if(jnddata.lt.jndcntrl
     .      .or.tab_8(9,i).eq.1.or.tab_8(9,j).eq.1
     .      .or.tab_8(9,i).eq.2.or.tab_8(9,j).eq.2) then
           dexy = dexyoa
         endif
cpppppppppp
ccc      print 1789, i,ctab1_i,(tab_8(ii,i),ii=2,3),nint(tab_8(4,i)),
ccc  .    tab_8(5,i),nint(tab_8(6,i)),(nint(-rab_8(ii,i)),ii=1,5),
ccc  .    nint(tab_8(9,i)),nint(tab_8(7,i))
c1789    format('I: ',I6,';{ ID: ',A8,'; LAT: ',F6.2,'; LON: ',F7.2,
ccc  .    '; RPRT DD HH.hhhhh ',I2.2,F8.5,'; ALT: ',I6,
ccc  .    '; RCPT YYYYMMDDHHMM: ',I4,4I2.2,'; file #',I2,'; AFWA:',I2,
ccc  .    '}')
ccc      print 1790, j,ctab1_j,(tab_8(ii,j),ii=2,3),nint(tab_8(4,j)),
ccc  .    tab_8(5,j),nint(tab_8(6,j)),(nint(-rab_8(ii,j)),ii=1,5),
ccc  .    nint(tab_8(9,j)),nint(tab_8(7,j))
c1790    format('J: ',I6,';{ ID: ',A8,'; LAT: ',F6.2,'; LON: ',F7.2,
ccc  .    '; RPRT DD HH.hhhhh ',I2.2,F8.5,'; ALT: ',I6,
ccc  .    '; RCPT YYYYMMDDHHMM: ',I4,4I2.2,'; file #',I2,'; AFWA:',I2,
ccc  .    '}')
cpppppppppp
         DO WHILE(NINT(ABS(TAB_8(2,I)-TAB_8(2,J))*10000.).LE.
     .    NINT(DEXY*10000.) .AND. JDUP(I).EQ.0)
cpppppppppp
ccc         print'("Record (J) may have been changed to",i6," - '//
ccc  .      'compare against unchanged record (I)",i6)', j,i
cpppppppppp
            if(jdup(j).ne.0) then
cpppppppppp
ccc            print *, '---> this report in record (J) has already ',
ccc  .          'been flagged as a duplicate - cycle'
cpppppppppp
               go to 800
            endif
            drhr = drhrn
            delv = delvn
            dell = dexyn
            if(jnddata.lt.jndcntrl
     .         .or.tab_8(9,i).eq.1.or.tab_8(9,j).eq.1
     .         .or.tab_8(9,i).eq.2.or.tab_8(9,j).eq.2) then
              DRHR = DRHRO
              DELV = DELVO
              dell = max(dexyo,dexyoa*max(tab_8(7,i),tab_8(7,j)))
            endif
      IF(NINT(ABS(TAB_8(2,I)-TAB_8(2,J))*10000.).LE.NINT(DELL*10000.)
     ..AND.NINT(ABS(TAB_8(3,I)-TAB_8(3,J))*10000.).LE.NINT(DELL*10000.)
     ..AND.NINT(ABS(TAB_8(4,I)-TAB_8(4,J))*100.).LE.NINT(DDAY*100.)
     ..AND.NINT(ABS(TAB_8(5,I)-TAB_8(5,J))*10000.).LE.NINT(DRHR*10000.)
     ..AND.NINT(ABS(TAB_8(6,I)-TAB_8(6,J))*100.).LE.NINT(DELV*100.))
     .THEN
cpppppppppp
ccc            print 1799, i,ctab1_i,(tab_8(ii,i),ii=2,3),
ccc  .          nint(tab_8(4,i)),tab_8(5,i),nint(tab_8(6,i)),
ccc  .          (nint(-rab_8(ii,i)),ii=1,5),nint(tab_8(9,i)),
ccc  .          nint(tab_8(7,i))
c1799          format('===> DUPL. FOUND:'/'   --> I: ',I6,';{ ID: ',A8,
ccc  .          '; LAT: ',F6.2,'; LON: ',F7.2,'; RPRT DD HH.hhhhh ',
ccc  .          I2.2,F8.5,'; ALT: ',I6,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
ccc  .          '; file #',I2,'; AFWA:',I2,'}')
ccc            print 1800, j,ctab1_j,(tab_8(ii,j),ii=2,3),
ccc  .          nint(tab_8(4,j)),tab_8(5,j),nint(tab_8(6,j)),
ccc  .          (nint(-rab_8(ii,j)),ii=1,5),nint(tab_8(9,j)),
ccc  .          nint(tab_8(7,j))
c1800          format('   --> J: ',I6,';{ ID: ',A8,'; LAT: ',F6.2,
ccc  .          '; LON: ',F7.2,'; RPRT',' DD HH.hhhhh ',I2.2,F8.5,
ccc  .          '; ALT: ',I6,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
ccc  .          '; file #',I2,'; AFWA:',I2,'}')
cpppppppppp

C .. if duplicate check is satisfied and report "I" is either from
C    AFWA or it is from a less-preferred file-type than
C    report "J" (e.g., "I" is an AIREP or PIREP and "J" is an AMDAR -or-
C    "I" is a PIREP and "J" is an AIREP), set its duplicate flag to 1
C    in order to throw it out thus retaining report "J" at this point
C    (regardless of whether or not it is from AFWA)
               IF(TAB_8(7,I).EQ.1) then
                 JDUP(I) = 1
cpppppppppp
ccc               print *, 'I tossed ==> AFWA'
cpppppppppp
               else if((tab_8(9,i).le.2.and.tab_8(9,j).ge.3) .or.
     .                 (tab_8(9,i).eq.2.and.tab_8(9,j).eq.1)) then
cpppppppppp
ccc               print *, 'I tossed ==> from less-preferred file-type',
ccc  .             ' than J'
cpppppppppp
                 jdup(i) = 1

               end if

C .. at this point, if duplicate check is satisfied and report "I"'s
C    duplicate flag is not set (meaning it is not from
C    AFWA -or- it is not from a less-preferred type), set report "J"'s
C    duplicate flag to 1 in order to throw it out (regardless of
C    whether or not it is from AFWA) thus retaining report "I"
               IF(JDUP(I).EQ.0) then
                 JDUP(J) = 1
cpppppppppp
ccc               print *, 'J tossed'
cpppppppppp
               end if

            ENDIF  ! endif for dupe test/comparison
  800       continue
            IF(K+1.GT.NTAB) CYCLE LOOP1
            J = IORD(K+1)
            tab1_j_8 = tab_8(1,j)
            K = K+1
         ENDDO
      ENDDO LOOP1
 
C  WRITE BACK THE DUP-CHECKED FILE(S)
C  ----------------------------------
 
      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

      DO I=1,NFILES
         IF(FILI(I)(1:4).NE.'NONE') THEN
cpppppppppp
cc    idate_last = -9999
cpppppppppp
            FILO = '.'//FILI(I)
            ISUB = IPTR(1,I)-1
            CALL CLOSBF(LUBFI)
            CALL CLOSBF(LUBFJ)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            OPEN(LUBFJ,FILE=FILO   ,FORM='UNFORMATTED')
            CALL OPENBF(LUBFI,'IN ',LUBFI)
            CALL OPENBF(LUBFJ,'OUT',LUBFI)
 
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
cpppppppppp
cc       print *, 'New message read in , NSUBS, IDATE = ',nsubs,idate
cpppppppppp

C  If no subsets in msg & dummy msgs not expected loop to next input msg
C  ---------------------------------------------------------------------

               IF(NSUBS.LE.0.AND.DUMMY_MSGS.NE.'YES')  CYCLE

               DUPES = .FALSE.

               IF(NSUBS.GT.0)  THEN
                  DO N=1,NSUBS
                     IF(ISUB+N.GT.NTAB) THEN
                        IDUP = 4
                     ELSE
                        IDUP = JDUP(ISUB+N)
                     ENDIF
                     NDUP(IDUP) = NDUP(IDUP)+1
                     ndup_file(idup,i) = ndup_file(idup,i)+1
                     IF(ISUB+N.LE.NTAB) THEN
                        IF(TAB_8(7,ISUB+N).EQ.1) then
                           NDUP(3) = NDUP(3)+1
                           ndup_file(3,i) = ndup_file(3,i)+1
                        endif
                     ENDIF
                     IF(IDUP.GT.0) DUPES = .TRUE.
                  ENDDO
               ENDIF

cpppppppppp
cc    print *, 'DUPES = ',dupes
cpppppppppp

               IF(DUPES) THEN
                  CALL OPENMB(LUBFJ,SUBSET,IDATE)
cpppppppppp
cc             if(idate.ne.idate_last)
cc   $         print *, 'NEW MESSAGE OPENED'
cc             idate_last = idate
cpppppppppp
                  DO WHILE(IFBGET(LUBFI).EQ.0)
                     ISUB = ISUB+1
                     IF(ISUB.GT.NTAB) THEN
                        IDUP = 4
                     ELSE
                        IDUP = JDUP(ISUB)
                     ENDIF
                     IF(IDUP.EQ.0) THEN
                        CALL COPYSB(LUBFI,LUBFJ,IRET) ! Copy non-dups
                     ELSE
                        IF(ISUB.LE.NTAB) THEN
                           IF(TAB_8(7,ISUB).EQ.1) then
                              NDUP(2) = NDUP(2)+1
                              ndup_file(2,i) = ndup_file(2,i)+1
                           endif
                        ENDIF
                        CALL COPYSB(LUBFI,00000,IRET) ! Toss dups
                     ENDIF
                  ENDDO
               ELSE

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
                  CALL COPYMG(LUBFI,LUBFJ)  ! Copy non-dups
                  ISUB = ISUB+NSUBS
               ENDIF
            ENDDO

            CALL CLOSBF(LUBFI)
            CALL CLOSBF(LUBFJ)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            OPEN(LUBFJ,FILE=FILO   ,FORM='UNFORMATTED')
            CALL COPYBF(LUBFJ,LUBFI)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')

            CALL MESGBC(LUBFI,MSGT,ICOMP)
            IF(ICOMP.EQ.1) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," MESSAGES   '//
     .          'C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",'//
     .          'I5/)', I,MSGT
            ELSE  IF(ICOMP.EQ.0) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," MESSAGES   '//
     .          'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND '//
     .          'IS",I5/)', I,MSGT
            ELSE IF(ICOMP.EQ.-1) THEN
               PRINT'(//"ERROR READING OUTPUT BUFR FILE",I2," - '//
     .          'MESSAGE COMPRESSION UNKNOWN"/)', I
            ELSE  IF(ICOMP.EQ.-3) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," DOES NOT EXIST"/)', I
            ELSE  IF(ICOMP.EQ.-2) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," HAS NO DATA MESSAGES"/'//
     .          '"FIRST MESSAGE TYPE FOUND IS",I5/)', I,MSGT
            ENDIF

            CLOSE(LUBFI)
         ENDIF
      ENDDO

 400  CONTINUE

C  GENERATE REPORT SUMMARY
C  -----------------------
 
      PRINT 300, ISUB,NDUP(4),MXTB,NTAB
  300 FORMAT(/'BUFR_DUPAIR READ IN A TOTAL OF',I8,' REPORTS'/
     .        '  A TOTAL OF ',I7,' REPORTS WERE SKIPPED DUE TO BEING ',
     .        'OVER THE LIMIT OF ',I7//
     .        'BUFR_DUPAIR CHECKED A TOTAL OF',I8,' REPORTS')
      do i = 1,nfiles
         if(ntab_file(i).gt.0) print 301, ntab_file(i),ctext_file(i),
     .    ndup_file(3,i)
  301 format('  THESE INCLUDE ',I8,' REPORTS FROM ',A19,', where ',i8,
     . ' are tagged as AFWA')
      enddo
      print'(54x,"total AFWA = "I6)', NDUP(3)

      print 302, ndup(0)
  302 format(/'NUMBER OF UNIQUE REPORTS WRITTEN OUT ...........',i7)
      do i = 1,nfiles
         if(ndup_file(0,i).gt.0) print 303, ndup_file(0,i),ctext_file(i)
      enddo
  303 format(10x,'THESE INCLUDE ',i8,' REPORTS FROM ',a19)

      print 304, ndup(1)
  304 format(/'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK (ALL SOURCES) .......',i7)
      do i = 1,nfiles
         if(ndup_file(1,i).gt.0) print 303, ndup_file(1,i),ctext_file(i)
      enddo

      print 305, ndup(2)
  305 format('      # of AFWA failing duplicate check ........',i7)
      do i = 1,nfiles
         if(ndup_file(2,i).gt.0) print 303, ndup_file(2,i),ctext_file(i)
      enddo

      print 306, ndup(1)-ndup(2)
  306 format('      # of non-AFWA failing duplicate check ....',i7)
      do i = 1,nfiles
         if(ndup_file(1,i)-ndup_file(2,i).gt.0) print 303,
     .    ndup_file(1,i)-ndup_file(2,i),ctext_file(i)
      enddo

C  END OF PROGRAM
C  --------------

      WRITE(60,'("ALL DONE")')
      CALL W3TAGE('BUFR_DUPAIR')
      STOP

C  ERROR EXITS
C  -----------

  901 CONTINUE

      PRINT *, '#####BUFR_DUPAIR - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPAIR')
      CALL ERREXIT(99)

      END

