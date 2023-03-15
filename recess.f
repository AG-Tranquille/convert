      PROGRAM RECESS
C  BY AL RUTLEDGE, USGS      2007 VERSION
C 
C  THIS PROGRAM PERFORMS ANALYSIS OF STREAMFLOW RECESSION USING A 
C  DAILY-VALUES RECORD OF STREAMFLOW.  MORE INFORMATION IS PROVIDED
C  IN THE BLOCK OF WRITE STATEMENTS BELOW.
C
C
C  Features of this version (and the version in 2003) that are
C  different from earlier versions --
c
c    (1) This one reads data in the flat-file format available
c        from USGS web sites.
c    (2) This one writes recession-segment data to a separate
c        file "recdata.txt" so the user can screen recession data
c        using separate computer graphics applications.
c    (3) Various changes in the naming of input and output files.
c
C  THIS PROGRAM OBTAINS THE DRAINAGE AREA FROM FILE "station.txt"
C
C  SEVERAL DECLARATION STATEMENTS PERTAIN TO ARRAY SIZES:
C          MAXIMUM NUMBER OF YEARS = 120.
C          MAXIMUM NUMBER OF DAYS = 44000.
C          MAXIMUM # OF DAYS IN ALL SELECTED RECESSION SEGMENTS = 3000.
C          RETRIEVE DAILY VALUES AFTER THE YEAR 1890.
      COMMON/BIG/Q(120,12,31)
      CHARACTER*1 FLAG(120,12)
      COMMON/BIG/Q1D(44000)
      COMMON/BIG/IYR1D(44000)
      COMMON/BIG/IMO1D(44000)
      COMMON/BIG/IDA1D(44000)
      REAL XX(3000), YY(3000), ZZ(3000)
      REAL FLOW(60), QLOG(60)
      INTEGER INDICAT(3000)
      INTEGER IYR(60), IMO(60), IDA(60)
      REAL Q
      REAL Q1D
      REAL QLOGMAX, QLOGMIN, MXLOGQC, MNLOGQC
      INTEGER IYR1D
      INTEGER IMO1D
      INTEGER IDA1D
      INTEGER PICKMO(12)
      CHARACTER*16 INFILE
      CHARACTER*16 INFILE1
      CHARACTER*16 OUTFIL1
      CHARACTER*16 OUTFIL2
      CHARACTER*80 PICK
      CHARACTER*80 YESNO
      CHARACTER*1 SEASON
      REAL QMAX, QMIN, KMAX, KMIN, KMED                                        C
      CHARACTER*15 STANUM                                                      C
      CHARACTER*9 LINE
C         CHARACTER*16 FNAME                                                   C
      CHARACTER*1 BLANK(80)
      REAL XMEANAR(50), YMEANAR(50), COEF1AR(50), COEF2AR(50)
      REAL XMNARAY(50), COFARAY(50), X(50), Y(50), K(50), DUMMY(50)
      INTEGER IMINAR(50), IMAXAR(50), IYRAR(50),IMOAR(50),IDAAR(50)
      INTEGER IPICK(50), ORIGNO(50)
      REAL FLONUM
      CHARACTER*5 AGENCY                                                       C
      CHARACTER*180 BUF_LINE                                                   C

 
      IBEFORE=1890
 
      OPEN (UNIT=12,FILE='recsum.txt')
    1 READ (12,21,END=2)
      GO TO 1
    2 CONTINUE

C      NOTE: SOME COMPILERS MIGHT REQUIRE A LINE HERE "BACKSPACE 12"
C              THIS SHOULD NOT BE DONE IF RUNNING ON A DG.

      BACKSPACE 12

      open (unit=13,file='index.txt')
    3 read (13,21,end=4)
      go to 3
    4 continue

      backspace 13


    5 FORMAT (2I6,3F11.2,1F8.2)
      WRITE (*,*) 'THIS PROGRAM READS A DAILY-VALUES FILE OF STREAM-  '
      WRITE (*,*) 'FLOW, FINDING PERIODS OF CONTINUOUS RECESSION. FOR '
      WRITE (*,*) 'EACH PERIOD, THE USER CAN SELECT A SEGMENT THAT IS '
      WRITE (*,*) 'INDICATIVE OF NEAR-LINEAR RECESSION WHEN GROUND-   '
      WRITE (*,*) 'WATER-HEAD PROFILES ARE STABLE.                    '
      WRITE (*,*) 'AFTER THIS PROCESS, THE PROGRAM GIVES OUTPUT       '
      WRITE (*,*) 'THAT CAN BE USED FOR THE FOLLOWING --              '
      WRITE (*,*) ' (1) DETERMINATION OF THE MEDIAN RECESSION INDEX,  '
      WRITE (*,*) '     IN DAYS PER LOG CYCLE. (THIS CAN BE USED WHEN '
      WRITE (*,*) '     THE RORABAUGH MODEL IS APPLIED FOR RECHARGE   '
      WRITE (*,*) '     ESTIMATION.)                                  '
      WRITE (*,*) ' (2) DERIVATION OF THE MASTER RECESSION CURVE,     '
      WRITE (*,*) '     WHICH CAN BE LINEAR OR NON-LINEAR.            '
      WRITE (*,*) '   '
      WRITE (*,*) 'THIS PROGRAM IS DOCUMENTED IN WRIR 98-4148       '
C
C------------------------- INITIALIZE VARIABLES : -------------------
      DO 10 IYEAR=1,120
      DO 10 IMONTH=1,12
      DO 10 IDAY=1,31
   10 Q(IYEAR,IMONTH,IDAY)=-999.0
      DO 11 I=1,12
   11 PICKMO(I)=0
      DO 12 I=1,80
   12 BLANK(I)= ' '
      II= 1
      XX(II)= -99.0
      YY(II)= -99.0
      NMRECES=0
      QLOGMAX=0.0
      QLOGMIN=10.0
C
   13 format (A12,1f8.2)
   14 format (f8.0)
   15 FORMAT (1I3, 2F9.3, 9X, 42A1)
   16 FORMAT (A12, 1F8.0)
   17 FORMAT (A12,A1,1X,1I4,'-',1I4,1I3,3F6.1,2F8.3,1F9.4,2F10.4)
   18 FORMAT (A26, 1F7.1, 33X, 1F7.1)
   19 FORMAT (1F10.5,1F15.3,3F8.1,10X,1I6,2I3)
   20 FORMAT (1I12, 2F10.4, 1I12, 3I8)
   21 FORMAT (A6)
   22 FORMAT (2X,I4)
   23 FORMAT (1F12.0)
   24 FORMAT (80X)
   25 FORMAT (I4)
   26 FORMAT (A4,1X,A8,1X,I4,1X,I2,1X,I2,1X,A)
   27 FORMAT (12F10.2)
   28 FORMAT (1I6, 2I3, 8F10.3)
   29 FORMAT (1I6, 2I3, 8(1F10.3,1A1) )
   30 format (1I10, 1f15.5)
   31 format (4f14.6, 1i14)
      IDISPLA = 20
      WRITE (*,*) '          '
      WRITE (*,*) 'AT THIS TIME THE USER MIGHT EXPAND THE WINDOW  '
      WRITE (*,*) 'DOWNWARD IF NEEDED.                            '
      WRITE (*,*) '  '
      WRITE (*,*) 'DURING PROGRAM EXECUTION, THE USER WILL BE ABLE TO VI
     $EW RECESSION DATA. '
      WRITE (*,*) 'THE RANGES OF DATA DISPLAY CAN BE CONTROLLED BY EDITI
     $NG FILE "ranges.txt". '
      WRITE (*,*) 'WHEN THIS IS DONE, SAVE AND EXIT ranges.txt BEFORE CO
     $NTINUING.    '

C
C -------------- READ THE DV FILE OF STREAMFLOW: ----------------------
C
      WRITE (*,*) 'GIVE THE NAME OF THE STREAMFLOW DAILY-VALUES  '            C
      WRITE (*,*) 'FILE  (the program is case-sensitive)         '            C
      write (*,*) '(Example file that is included: "Indian.txt") '            C
      READ (*,'(A)') INFILE

      WRITE (*,*) 'READING FILE NAMED ',INFILE
      OPEN (UNIT=9, FILE=INFILE)
      IYEAR=1
   33 READ (9,21) LINE
      IF( LINE.NE.'agency') THEN
            GO TO 33
        END IF

      READ (9,21) LINE

      IFRSTYR=0


   35 READ (9,'(A)',END=37) BUF_LINE                                          C
      DO 36 J=1,180
      IF (BUF_LINE(J:J).EQ.achar(9)) BUF_LINE(J:J)= ','                       2007
   36 IF (BUF_LINE(J:J).EQ.'-') BUF_LINE(J:J)= ','                            2007
      READ (BUF_LINE,*,END=37) AGENCY,STANUM,IYEAR,IMONTH,IDAY,FLONUM         C


      IF(IFRSTYR.EQ.0) IFRSTYR=IYEAR                                          C
      IYEAR = IYEAR-IBEFORE                                                   C


      Q(IYEAR,IMONTH,IDAY) = FLONUM

      FLONUM = -999.0                                                         2007

      GO TO 35
   37 CONTINUE

      ILSTYR= IYEAR+IBEFORE
      WRITE (*,*) 'FIRST YEAR IN RECORD = ', IFRSTYR
      WRITE (*,*) ' LAST YEAR IN RECORD = ', ILSTYR


c
c --- flag nonexistent dates with flow=-9999 -----
c
      do 38 IYEAR=IFRSTYR, ILSTYR
      DO 38 IMONTH=1,12
      DO 38 IDAY=1,31
         IF((IMONTH.EQ.2).AND.(IDAY.GT.29)) THEN
            Q(IYEAR-IBEFORE,IMONTH,IDAY)= -9999.0
          END IF
         IF((IMONTH.EQ.2).AND.(IDAY.EQ.29)) THEN
           IDIV=INT((IYEAR)/4.0)
           XDIV=(IYEAR)/4.0
           DIFFER=ABS(IDIV-XDIV)
           IF(DIFFER.GT.0.1) THEN
               Q(IYEAR-IBEFORE,IMONTH,IDAY)= -9999.0
             END IF
          END IF
         IF(IDAY.EQ.31) THEN
              IF((IMONTH.EQ.4).OR.(IMONTH.EQ.6).OR.(IMONTH.EQ.9)
     $         .OR.(IMONTH.EQ.11)) THEN
                    Q(IYEAR-IBEFORE,IMONTH,IDAY)= -9999.0
               END IF
          END IF
   38 CONTINUE




      WRITE (*,*) '                 MONTH  '
      WRITE (*,*) ' YEAR   J F M A M J J A S O N D'

      IFRSTYR= IFRSTYR-IBEFORE
      ILSTYR= ILSTYR-IBEFORE

      DO 72 IYEAR= IFRSTYR, ILSTYR

           DO 55 IMONTH=1,12
   55      FLAG(IYEAR,IMONTH)='.'
           DO 70 IMONTH=1,12
             DO 60 IDAY=1,31
               IF(Q(IYEAR,IMONTH,IDAY).EQ.-999) FLAG(IYEAR,IMONTH)='X'
               IF(Q(IYEAR,IMONTH,IDAY).EQ.-99) FLAG(IYEAR,IMONTH)='X'
   60         CONTINUE
   70         CONTINUE
            WRITE (*,73) IYEAR+IBEFORE, (FLAG(IYEAR,IMONTH),IMONTH=1,12)

   72 CONTINUE

        WRITE (*,*) ' '
        WRITE (*,*) ' COMPLETE RECORD = .      INCOMPLETE = X  '
        WRITE (*,*) ' '
   73 FORMAT (1I6, 2X, 12A2)
C
      CLOSE (9,STATUS='KEEP')




C
C ------------READ LIST FILE GIVING STATION PROPERTIES: --------------
C
C           WRITE (*,*) 'READING FILE "station.txt"  '                  C
C           OPEN (UNIT=8,FILE='station.txt')                            C
C           DO 78 I=1,10                                                C
C        78 READ (8,*)                                                  C
C        80 READ(8,16) FNAME, DA                                        C
C           IF(FNAME.NE.INFILE) GO TO 80                                C
C           WRITE (*,*) ' '                                             C
C           WRITE (*,*) 'STREAMFLOW FILE NAME:', FNAME                  C
C           WRITE (*,*) 'DRAINAGE AREA:', DA                            C
C           CLOSE (8,STATUS='KEEP')                                     C
C           WRITE (*,*) ' '                                             C
C
  100 CONTINUE
C
C ----- OBTAIN USER SPECIFICATIONS ABOUT RECESSION PERIODS DESIRED: ----
C
      WRITE (*,*) 'START IN WHICH YEAR? '
      READ (*,*) IYEARST
      WRITE (*,*) 'END IN WHICH YEAR? '
      READ (*,*) IYEAREN
      IIMAX=0
      DO 110 IIYR=IYEARST,IYEAREN
           IDIV=INT(IIYR/4.0)
           XDIV=IIYR/4.0
           DIFFER=ABS(IDIV-XDIV)
           IF(DIFFER.LT.0.1) THEN
               IIMAX=IIMAX+366
             ELSE
               IIMAX=IIMAX+365
            END IF
  110 CONTINUE
      IYEARST= IYEARST-IBEFORE
      IYEAREN= IYEAREN-IBEFORE
      WRITE (*,*) 'ONLY SELECT RECESSIONS BEGINNING IN PARTICULAR '
      WRITE (*,*) 'MONTHS. ENTER NUMBER OF MONTHS SELECTED: '
      READ (*,*) NMONTHS
      IF (NMONTHS.EQ.12) THEN
          DO 120 I=1,12
  120        PICKMO(I)=I
        ELSE
          WRITE (*,*) 'ENTER MONTHS  (enter 1 for Jan, 2 for Feb, etc.,'
          WRITE (*,*) '               and follow each with "enter" key)'
          write (*,*) ' '
          DO 130 I=1,NMONTHS
  130     READ (*,*) PICKMO(I)
       END IF
  135 CONTINUE
      WRITE (*,*) 'ENTER A SINGLE LETTER TO INDICATE WHAT SEASON, IF '
      WRITE (*,*) 'ANY, THIS REPRESENTS.                             '
      WRITE (*,*) ' '
      WRITE (*,*) ' (ENTER: w=WINTER, v=SPRING, s=SUMMER, f=FALL, or '
      WRITE (*,*) '  n=NO PARTICULAR SEASON)  '
      READ (*,'(A)') SEASON
      IF(SEASON.NE.'w'.AND.SEASON.NE.'v'.AND.SEASON.NE.'s'.AND.
     $   SEASON.NE.'f'.AND.SEASON.ne.'n') THEN
          WRITE (*,*) 'OPTION NOT RECOGNIZED.'
          GO TO 135
       END IF
      WRITE (*,*) 'IN ORDER THAT THE PROGRAM WILL DETECT A RECESSION '
      WRITE (*,*) 'SEGMENT, HOW MANY DAYS OF RECESSION ARE REQUIRED? '
      READ (*,*) IFARCRI
C
C --------------- WRITE HEADINGS IN OUTPUT FILES: -------------------
C
      OUTFIL1= 'x'//SEASON//'.'//INFILE//'.txt'
      OUTFIL2= 'y'//SEASON//'.'//INFILE//'.txt'
      OPEN (UNIT=10, FILE=OUTFIL1)
      OPEN (UNIT=11, FILE=OUTFIL2)

      WRITE (11,*) ' FILE ', outfil2, '--  UNIT 11 OUTPUT OF RECESS.F:'
      WRITE (11,*) ' INPUT DATA FILE FOR THIS SESSION: ', INFILE
      write (11,*) ' Tpeak is the time since the last peak  '
      write (11,*) ' Tmrc is the time on the Master Recession Curve '
      write (11,*) ' LogQ is the log of flow '
      write (11,*) ' Q is the flow '
      write (11,*) ' Seq# is the sequence number in which the segment '
      write (11,*) ' was selected.  '
      WRITE (11,*) '----------------------------------------------------
     $-------------------'
      WRITE (10,*) ' FILE ', outfil1,'--  UNIT 10 OUTPUT OF RECESS.F '
      WRITE (10,*) ' INPUT FILE = ', INFILE
      WRITE (10,*) ' START = ', IYEARST+IBEFORE
      WRITE (10,*) ' END =   ', IYEAREN+IBEFORE
      WRITE (10,*) ' DAYS OF RECESSION REQUIRED FOR DETECTION=',
     $  IFARCRI
      WRITE (10,*) ' MONTHS SELECTED:'
      DO 140 I=1, NMONTHS
  140 WRITE (10,*) '  ', PICKMO(I)
      WRITE (10,*) ' '
      WRITE (10,*) '----------------------------------------------------
     $-------------------'
      WRITE (10,*) '              RECESSION PERIODS INITIALLY SELECTED:'
      WRITE (10,*) '   LOG Q       RECESS.INDEX     TIME SINCE PEAK    .
     $       DATE OF PEAK '
      WRITE (10,*) '   (MEAN)    ( -dT/d(LogQ) ) (START)(MIDDLE)(END)  .
     $        (yr, mo, d) '
C
C ------ ASSIGN VALUES TO 1-DIMENSIONAL ARRAYS OF DISCHARGE AND DATE: ----
C
      ICOUNT= 0
      DO 180 IYEAR= IYEARST, IYEAREN
      DO 180 IMONTH= 1,12
      DO 180 IDAY= 1, 31
          SFLOW= Q(IYEAR,IMONTH,IDAY)
          IF(SFLOW.EQ.-99.OR.SFLOW.EQ.-999.OR.SFLOW.EQ.-9999) GO TO 180
          ICOUNT= ICOUNT + 1
          Q1D(ICOUNT)= SFLOW
          IYR1D(ICOUNT)= IYEAR
          IMO1D(ICOUNT)= IMONTH
          IDA1D(ICOUNT)= IDAY
  180 CONTINUE
      IF(QMIN.LT.0.001) THEN
          QMIN= 1.0
          WRITE (*,*) ' '
          WRITE (*,*) 'FOR THIS STATION, THE MINIMUM DISCHARGE IS ZERO.'
          WRITE (*,*) 'TO AVOID PROBLEMS WITH THE LOG FUNCTION, THE    '
          WRITE (*,*) 'PROGRAM WILL SHOW A MINIMUM DISCHARGE ON GRAPHS '
          WRITE (*,*) 'OF 1.0 CFS ( LOG Q = 0 ).  THIS CAN BE CHANGED  '
          WRITE (*,*) 'DURING INTERACTIVE DISPLAY IF THE USER WANTS.   '
       END IF

C
C ---------------------- LOCATE A PEAK: ------------------------------
C
      ICOUNT=1
  200 ICOUNT= ICOUNT + 1
      IF(ICOUNT.GT.IIMAX) GO TO 250
      IF(IYR1D(ICOUNT).GT.IYEAREN) GO TO 250
      OK=0
      DO 205 I=1,12
  205     IF(IMO1D(ICOUNT).EQ.PICKMO(I)) OK=1
      IF(OK.EQ.0) GO TO 200
      IF(Q1D(ICOUNT).LE.Q1D(ICOUNT-1).OR.Q1D(ICOUNT).LE.Q1D(ICOUNT+1))
     #                                                GO TO 200
      IPEAK= ICOUNT
      OK=1
C
C ------------- ANALYZE THE RECESSION AFTER THE PEAK: ---------------
C
  210 ICOUNT= ICOUNT+1
         IF (ICOUNT.GT.IIMAX) GO TO 250
         IF (INT(100*Q1D(ICOUNT)).GT.INT(100*Q1D(ICOUNT-1))) OK=0
         IHOWFAR= ICOUNT-IPEAK-1
         IF (OK.EQ.1) GO TO 210
         IF (IHOWFAR.LT.IFARCRI.AND.OK.EQ.0) THEN
             ICOUNT= ICOUNT-1
             GO TO 200
             END IF
         IF (IHOWFAR.GE.IFARCRI.AND.OK.EQ.0) THEN
             DO 215 IT= 1,60
                 FLOW(IT)= 0.0
                 QLOG(IT)= -99.9
                 IYR(IT)=0
                 IMO(IT)=0
                 IDA(IT)=0
  215           CONTINUE
             NUM= ICOUNT-IPEAK-1
             IF(NUM.GT.60) THEN
                 NUM=60
                 ICOUNT= IPEAK+60
              END IF
               IMIN=1
               IMAX=NUM
               DO 220 IT=1,NUM
                  I= IT+IPEAK
                  FLOW(IT)= Q1D(I)
                  IF(Q1D(I).EQ.0.0) THEN
                      QLOG(IT)= -88.8
                    ELSE
                      QLOG(IT)= LOG(Q1D(I)) / 2.3025851
                   END IF
                  IYR(IT)= IYR1D(I) + IBEFORE
                  IMO(IT)= IMO1D(I)
                  IDA(IT)= IDA1D(I)
  220           CONTINUE
              WRITE (*,*) ' '
              WRITE (*,*) ' '
              WRITE (*,*) '---------------------------------------------
     $------------------'
              WRITE(*,*) 'NUMBER OF RECESSION PERIODS USED SO FAR = ',
     $                     NMRECES
              WRITE(*,*) 'DATE OF NEW PEAK =', IYR1D(IPEAK)+IBEFORE,
     $                              IMO1D(IPEAK), IDA1D(IPEAK)
              WRITE(*,*) 'PERIOD OF SUBSEQUENT RECESSION (DAYS) =', NUM
  230      CONTINUE
C
C ------------------- OBTAIN OPTION FROM USER:----------------------
C
            write (*,*) '                                              '
            write (*,*) '                                              '
            WRITE (*,*) 'OPTIONS FOR DISPLAY USING THIS WINDOW --      '
            WRITE (*,*) ' g  -- graphical display                      '
            write (*,*) ' t  -- tabular display of data                '
            write (*,*) 'Recession data, on file "recdata.txt" can be di
     $splayed outside this '
            write (*,*) 'window using the "recplot" application.  Data d
     $isplay ranges can be '
            write (*,*) 'changed in file "ranges.txt".  Save and exit ra
     $nges.txt before continuing.'
            write (*,*) '                                              '
            write (*,*) 'OPTIONS FOR PROGRAM ACTION --                 '
            write (*,*) ' c  -- choose first + last days of the        '
            write (*,*) '       recession segment.                     '
            write (*,*) ' b  -- un-do the effect of option c.          '
            write (*,*) ' a  -- advance to next recession period       '
            write (*,*) '       (do not use this one)                  '
            write (*,*) ' r  -- perform regression, store results in   '
            write (*,*) '       memory, and advance to next recession  '
            write (*,*) ' q --  quit.  This option is used to exit the '
            write (*,*) '       selection of recession segments.       '

      open (unit=22, file='recdata.txt')
        write (22,*) 1+IMAX-IMIN
        do 231 IG=IMIN,IMAX
           YGRAPH= QLOG(IG)
           IF (YGRAPH.LT.-80) YGRAPH= -2
           WRITE (22,30) IG, YGRAPH
  231 continue
      close (22,status='keep')


            READ (*,'(A)') PICK


            IF (PICK.EQ.'t'.OR.PICK.EQ.'t2') THEN

                open (unit=23, file='ranges.txt')
                  read (23,*)
                  read (23,*) IDISPLA
                close (23,status='keep')

                CALL TABLE(QLOG,FLOW,IYR,IMO,IDA,IPEAK,IMIN,IMAX,PICK,
     $                      IDISPLA)


            write (*,*) '                                              '
            write (*,*) 'THIS --- INDICATES A DAY OUTSIDE THE PERIOD OF
     $ RECESSION, OR A DAY OUTSIDE '
            write (*,*) 'OF THE SEGMENT THAT HAS BEEN SELECTED. '


                GO TO 230

              ELSEIF(PICK.EQ.'g'.OR.PICK.EQ.'g2') THEN

                open (unit=23, file='ranges.txt')
                  read (23,*)
                  read (23,*) IDISPLA
                  read (23,*) XLGQMIN
                  read (23,*) XLGQMAX
                close (23,status='keep')


                CALL GRAPH(QLOG,FLOW,IYR,IMO,IDA,IPEAK,IMIN,IMAX,
     $                     XLGQMAX,XLGQMIN,PICK,IDISPLA)


            write (*,*) '                                              '
            write (*,*) 'THIS * REPRESENTS FLOW.  THIS --- INDICATES A D
     $AY OUTSIDE THE PERIOD  '
            write (*,*) 'OF RECESSION, OR A DAY OUTSIDE OF THE SEGMENT T
     $HAT HAS BEEN SELECTED. '
            write (*,*) 'THIS ::::: INDICATES FLOW OUTSIDE OF PLOTTING R
     $ANGE.                  '



                GO TO 230
              ELSEIF(PICK.EQ.'c') THEN
  232           continue
                WRITE (*,*) 'ENTER THE FIRST DAY OF THE SEGMENT ********
     $*** enter a NUMBER ******'
                READ (*,*,ERR=232) IMIN
  234           continue
                WRITE (*,*) 'ENTER THE LAST DAY OF THE SEGMENT *********
     $*** enter a NUMBER ******'
                READ (*,*,ERR=234) IMAX
                GO TO 230
              ELSEIF(PICK.EQ.'b') THEN
                IMIN= 1
                IMAX= ICOUNT-IPEAK-1
                GO TO 230
              ELSEIF(PICK.EQ.'a') THEN
                ICOUNT= ICOUNT-1
                GO TO 200

              ELSEIF (PICK.EQ.'r') THEN

                 IF(QLOG(IMIN).EQ.QLOG(IMAX)) THEN
                     WRITE (*,*) 'THE PROGRAM SKIPPED THIS RECESSION'
                     WRITE (*,*) 'PERIOD BECAUSE FLOW DID NOT CHANGE'
                     WRITE (*,*) '(RECESSION INDEX UNDEFINED).'
                     ICOUNT=ICOUNT-1
                     GO TO 200
                    END IF
                 IF(IMAX-IMIN.GT.49) THEN
                     WRITE (*,*) 'THE NUMBER OF DAYS SELECTED SHOULD BE'
                     WRITE (*,*) 'LESS THAN 51 BEFORE PICKING OPTION r'
                     GO TO 230
                    END IF

                NMRECES= NMRECES+1
                 IF(NMRECES.GT.50) THEN
                     WRITE (*,*) 'YOU HAVE ANALYZED THE MAXIMUM NUMBER'
                     WRITE (*,*) 'OF RECESSION PERIODS.'
                     NMRECES= NMRECES-1
                     GO TO 250
                    END IF
                I=0
                DO 240 IT= IMIN, IMAX
                I=I+1
                II=II+1
                 IF(II.GT.3000) THEN
                     WRITE (*,*) 'THE TOTAL NUMBER OF DAYS IN ALL '
                     WRITE (*,*) 'SELECTED RECESSION PERIODS EXCEEDS'
                     WRITE (*,*) 'THE LIMIT OF 3000.   '
                     GO TO 250
                    END IF
                INDICAT(II)= NMRECES
                X(I)= QLOG(IT)
                Y(I)= REAL(IT)
                XX(II)= QLOG(IT)
                YY(II)= REAL(IT)
                IF(QLOG(IT).GT.QLOGMAX) QLOGMAX= QLOG(IT)
                IF(QLOG(IT).LT.QLOGMIN) QLOGMIN= QLOG(IT)
  240           CONTINUE
                II=II+1
                XX(II)= 0.0
                YY(II)= 0.0
                NSELECT= I
                WRITE (*,*) '     DAYS         LOG Q                  '
                WRITE (*,*) '   ( Y(I) )      ( X(I) )               I'
                XTOTAL= 0.0
                YTOTAL= 0.0
                DO 245 I=1,NSELECT
                XTOTAL= XTOTAL + X(I)
                YTOTAL= YTOTAL + Y(I)
  245           WRITE (*,*)  Y(I), X(I), I
                XMEAN= XTOTAL/NSELECT
                YMEAN= YTOTAL/NSELECT
                COEFF1=0.0
                COEFF2=0.0
                CALL REGRES2(X,Y,NSELECT,COEFF1,COEFF2)

                WRITE (*,*) ' BEST-FIT EQUATION:'
                WRITE (*,248) ' T = ( ', COEFF1,'* LOGQ )  +  ', COEFF2
                WRITE (*,*) '  DAYS/LOG CYCLE=', -1*COEFF1
  248           FORMAT (A,G12.4,A,G12.4)
                WRITE (*,*) '  MEAN LOG Q = ', XMEAN
                WRITE (*,*) ' '
                WRITE (*,*) ' '
                WRITE(10,19) XMEAN, -1*COEFF1, REAL(IMIN), YMEAN, 
     $          REAL(IMAX),IYR1D(IPEAK)+IBEFORE,IMO1D(IPEAK), 
     $               IDA1D(IPEAK)
                XMEANAR(NMRECES)= XMEAN
                YMEANAR(NMRECES)= YMEAN
                COEF1AR(NMRECES)= COEFF1
                COEF2AR(NMRECES)= COEFF2
                IMINAR(NMRECES)=  IMIN
                IMAXAR(NMRECES)=  IMAX
                IYRAR(NMRECES)=   IYR1D(IPEAK) + IBEFORE
                IMOAR(NMRECES)=   IMO1D(IPEAK)
                IDAAR(NMRECES)=   IDA1D(IPEAK)
                   IF (NMRECES.EQ.50) THEN
                       WRITE (*,*)  ' '
                       WRITE (*,*) 'YOU HAVE ANALYZED 50 RECESSIONS.'
                       WRITE (*,*) 'THIS IS THE MAXIMUM ALLOWABLE.'
                       WRITE (*,*) ' '
                       WRITE (*,*) ' '
                     END IF
                 ICOUNT= ICOUNT-1
                GO TO 200
              ELSEIF (PICK.EQ.'q') THEN
                GO TO 250
              ELSE
                WRITE (*,*) 'OPTION NOT RECOGNIZED. CHOOSE AGAIN.'
                GO TO 230
             END IF
          END IF
  250 CONTINUE
C
C ----------CONTINUE AFTER RECESSION PERIODS HAVE BEEN SELECTED:-------
C
      NUMBRII= II
      IIDV=0
      DO 270 III=1,NUMBRII
         IF(XX(III).NE.0.0.AND.YY(III).NE.0.0.AND.XX(III).NE.-99.AND.
     $                             YY(III).NE.-99) THEN
               IIDV=IIDV+1
          END IF
  270 CONTINUE
      WRITE (10,*) 'TOTAL NUMBER OF DAILY VALUES OF STREAMFLOW THAT WERE
     $ USED, FOR ALL RECESSION'
      WRITE (10,*) 'PERIODS INITIALLY SELECTED = ', IIDV
  280 CONTINUE
      WRITE (*,*) 'DO YOU WANT TO ANALYZE THE RECESSIONS SELECTED? (y OR
     $ n) '
      READ(*,'(A)') PICK
      IF(PICK.EQ.'n') THEN
            GO TO 700
         ELSEIF(PICK.EQ.'y') THEN
            GO TO 290
         ELSE
            WRITE (*,*) 'OPTION NOT RECOGNIZED.'
            GO TO 280
         END IF
  290  CONTINUE
C
C ----- DETERMINE MAX AND MIN K AND TRANSFER LOGQ AND K TO OTHER ----
C ---------- VARIABLES FOR LISTING THEM BY DECREASING LOGQ:  --------
        SLOPEMX= 0.0
        SLOPEMN= 2000.0
        DO 300 I=1,NMRECES
        SLOPE= -1*COEF1AR(I)
        IF(SLOPE.GT.SLOPEMX) SLOPEMX=SLOPE
        IF(SLOPE.LT.SLOPEMN) SLOPEMN=SLOPE
        ORIGNO(I)=I
        XMNARAY(I)= XMEANAR(I)
  300   COFARAY(I)= COEF1AR(I)
        CALL ORDER(NMRECES,XMNARAY,COFARAY,ORIGNO)
        WRITE (10,*) 'NUMBER OF RECESSION PERIODS INITIALLY SELECTED=',
     $          NMRECES
        WRITE (10,*) 'MAXIMUM LOG Q FOR ALL RECESSIONS=', QLOGMAX
        WRITE (*,*)  'MAXIMUM LOG Q FOR ALL RECESSIONS=', QLOGMAX
        WRITE (10,*) 'MINIMUM LOG Q FOR ALL RECESSIONS=', QLOGMIN
        WRITE (*,*)  'MINIMUM LOG Q FOR ALL RECESSIONS=', QLOGMIN
        WRITE (10,*) ' '
        WRITE (10,*) '--------------------------------------------------
     $------------------'
        WRITE (10,*) '       RECESSION PERIODS AFTER SORTING BY LOG Q:'
        WRITE (10,*) 'ORIG.                              GRAPHIC OF RECE
     $SSION INDEX (K)'
        WRITE (10,18) 'NUMBER LOG Q     K        ', SLOPEMN, SLOPEMX
       NOWRITE=1
  305   CONTINUE
            WRITE (*,*)  'ORIG.  ORDERED                    GRAPHIC OF R
     $ECESSION INDEX (K)'
            WRITE (*,18)  'NUMBER  LOG Q    K        ', SLOPEMN, SLOPEMX
            DO 310 I=NMRECES,1,-1
            IPICK(I)= 1
            SLOPE= -1.0*COFARAY(I)
            DIFF= SLOPE - SLOPEMN
            NUMBLNK= INT( DIFF*40/ (SLOPEMX-SLOPEMN) )
            WRITE (*,15)  ORIGNO(I), XMNARAY(I), COFARAY(I),
     $                (BLANK(J),J=1,NUMBLNK), '*'
              IF (NOWRITE.EQ.1) THEN
                 WRITE (10,15) ORIGNO(I), XMNARAY(I), COFARAY(I),
     $                   (BLANK(J),J=1,NUMBLNK), '*'
               END IF
  310       CONTINUE
            NOWRITE=0


C
C -------- SELECT DATA LINES TO BE DELETED BEFORE REGRESSION:  --------

        WRITE (*,*) 'BEFORE OBTAINING THE LEAST-SQUARES BEST FIT'
        WRITE (*,*) 'EQUATION FOR K (DELTA T/DELTA LOG Q) VERSUS'
        WRITE (*,*) 'LOG Q, HOW MANY RECESSIONS SHOULD BE ELIMINATED?'
        READ (*,*) NUMELIM
        WRITE (10,*) 'BEFORE OBTAINING THE LEAST-SQUARES BEST FIT '
        WRITE (10,*) 'EQUATION FOR K (DELTA T/DELTA LOG Q) VERSUS'
        WRITE (10,*) 'LOG Q, THIS NUMBER OF RECESSIONS WAS ELIMINATED:',
     $           NUMELIM
        IF(NUMELIM.EQ.0) GO TO 330
           WRITE (11,*) 'NOTE THAT THESE RECESSIONS, IDENTIFIED BY   '
           WRITE (11,*) 'THEIR ORIGINAL SEQUENTIAL NUMBERS, WERE     '
           WRITE (11,*) 'DELETED FROM ANALYSIS BEFORE DETERMINING   '
           WRITE (11,*) 'BEST-FIT EQUATIONS:'
        DO 320 J=1,NUMELIM
           WRITE (*,*) 'ENTER RECESSION TO ELIMINATE (ENTER ITS "ORIGINA
     $L NUMBER") '
           READ (*,*) ITHIS
           WRITE (11,*) '             ', ITHIS
           DO 315 I=1,NMRECES
               IF(ORIGNO(I).EQ.ITHIS) THEN
                   ORIGNO(I)=0
                  END IF
  315        CONTINUE
  320    CONTINUE
  330    CONTINUE
C
C ----ASSIGN VALUES TO X AND Y TO BE SENT TO THE REGRESSION SUBROUTINE:----
        NUM=0
        DO 340 I=1,NMRECES
           IF(ORIGNO(I).EQ.0) GO TO 340
           NUM= NUM + 1
           X(NUM)= XMNARAY(I)
           Y(NUM)= COFARAY(I)
           ORIGNO(NUM)=ORIGNO(I)
  340    CONTINUE
        NMRECES= NUM
        WRITE (*,*) '    X               Y'
        DO 350 I=NMRECES,1,-1
  350   WRITE (*,*) X(I), Y(I)
C
C  --------------- SHOW ORDERED DATA, AFTER ELIMINATION:  --------------
C
      WRITE (10,*) ' '
      WRITE (10,*) '----------------------------------------------------
     $-------------------'
      WRITE (10,*) '        RECESSION PERIODS LEFT AFTER ELIMINATION: '
      WRITE (10,*) 'ORIG.  ORDERED                      GRAPHIC OF RECES
     $SION INDEX (K)'
      WRITE (10,18) 'NUMBER  LOG Q    K         ', SLOPEMN, SLOPEMX
      DO 352 I=NMRECES,1,-1
         SLOPE=-1.0*Y(I)
         DIFF= SLOPE-SLOPEMN
         NUMBLNK= INT(DIFF*40/(SLOPEMX-SLOPEMN))
  352   WRITE(10,15) ORIGNO(I), X(I), Y(I),
     $               (BLANK(J),J=1,NUMBLNK), '*'
      MNLOGQC= X(1)
      MXLOGQC= X(NMRECES)
      WRITE (10,*) 'AMONG THE SELECTED RECESSION PERIODS, THESE ARE THE'
      WRITE (10,*) 'MIN AND MAX VALUES OF LOGQ FOR WHICH K (DAYS PER '
      WRITE (10,*) 'LOGQ) WAS CALCULATED:',MNLOGQC, MXLOGQC
C
C-- PERFORM REGRESSION AND WRITE BEST EQUATION FOR K AS FUNCT. OF DISCHARGE
C
        COEFFA= 0.0
        COEFFB= 0.0
        CALL REGRES2(X,Y,NUM,COEFFA,COEFFB)
  353   FORMAT (A,F8.2,A,F8.2)
  354   FORMAT (A,F8.2,A,F8.2,A,F8.2)
        WRITE (10,*) ' '

        WRITE (10,*) '--------------------------------------------------
     $------------------'
        WRITE (10,*) 'BEST-FIT LINEAR EQUATION FOR K VS. LOG Q:'
        WRITE (10,353) 'DELTA T/DELTA LOGQ = (',COEFFA,' * LOGQ ) + ',
     $                          COEFFB
        WRITE (10,*) '  RESULTS OF THIS EQUATION:'
        WRITE (10,*) '             '
        WRITE (10,*) '                                    GRAPHIC OF REC
     $ESSION INDEX (K)'
        WRITE (10,18) '      LOG Q               ', SLOPEMN, SLOPEMX
        XLOGQ= MXLOGQC
  370   CONTINUE
        SLOPE= -1*COEFFA*XLOGQ - 1*COEFFB
        DIFF= SLOPE-SLOPEMN
        NUMBLNK= INT(DIFF*40/(SLOPEMX-SLOPEMN))
         IF(NUMBLNK.LT.0) THEN
             WRITE (10,*) XLOGQ, SLOPE, '    '
            ELSE
             WRITE (10,*) XLOGQ, SLOPE, (BLANK(J), J=1,NUMBLNK), '*'
          END IF
        XLOGQ= XLOGQ - 0.05
        IF(XLOGQ.GT.MNLOGQC) GO TO 370

C ---- AFTER INTEGRATION WRITE EQUATION FOR TIME AS FUNCTION OF DISCHARGE: ---
        COEFFC= -0.5*COEFFA*MXLOGQC**2 - COEFFB*MXLOGQC
        WRITE (10,*) ' '
        WRITE (10,*) '--------------------------------------------------
     $-------------------'
        WRITE (10,*) 'AFTER INTEGRATION, THE FOLLOWING EQUATION IS '
        WRITE (10,*) 'OBTAINED. IT GIVES TIME (IN DAYS) AS A FUNCTION'
        WRITE (10,*) 'OF LOG Q. INITIAL CONDITION IS T=0 AT LOG Q= THE'
        WRITE (10,*) 'MAXIMUM LOG Q FOR WHICH A VALUE OF K WAS '
        WRITE (10,*) 'CALCULATED.'
        WRITE (10,354) ' T =',COEFFA/2,' * LOGQ**2  +', COEFFB,' * LOGQ
     $+',    COEFFC
        WRITE (10,*) '         RESULTS OF THIS EQUATION:'
        WRITE (10,*) '                                                GR
     $APHIC OF TIME:'
        TIMEMAX= 0.5*COEFFA*QLOGMIN**2 + COEFFB*QLOGMIN + COEFFC
        WRITE (10,*) '    TIME(D)         LOG Q           Q       0.0 '
        XLOGQ= MXLOGQC
  380   CONTINUE
        T= 0.5*COEFFA*XLOGQ**2 + COEFFB*XLOGQ + COEFFC
        XQ= 10.0**XLOGQ
        NUMBLNK= INT(T*25/TIMEMAX)
        WRITE (10,*) T, XLOGQ, XQ, (BLANK(J),J=1,NUMBLNK), '*'
        XLOGQ= XLOGQ-0.05
        IF(XLOGQ.GT.MNLOGQC) GO TO 380
C
C ------------   DETERMINE MAX, MIN, AND MEDIAN K: ---------------------------
C
      DO 390 I=1,NMRECES
         K(I)= Y(I)*(-1)
  390    DUMMY(I)= 1.0
      CALL ORDER(NMRECES,K,DUMMY,ORIGNO)
      KMAX= K(NMRECES)
      KMIN= K(1)
      IDOWN= 0
      IUP= NMRECES + 1
      ICNT=0
  395 CONTINUE
        ICNT=ICNT+1
        IF(ICNT.GT.50) THEN
           WRITE (*,*) 'PROBLEMS WITH DETERMINATION OF MEDIAN'
           GO TO 400
         END IF
        IF(IUP-IDOWN.EQ.2) THEN
           KMED= K((IUP+IDOWN)/2)
           GO TO 400
          ELSEIF(IUP-IDOWN.EQ.1) THEN
           KMED= (K(IUP)+K(IDOWN)) / 2
           GO TO 400
          ELSE
           IDOWN= IDOWN+1
           IUP= IUP-1
           GO TO 395
         END IF
  400 CONTINUE

C ------------------- WRITE RAW RECESSION DATA TO "y-file"  ----------------

      WRITE (11,*) '----------------------------------------------------
     $------------------'


      WRITE (11,*) '     Tpeak            Tmrc          LogQ           Q
     $               Seq#  '


      II= NUMBRII + 1
  520 II= II-1
      IF(XX(II).EQ.0.0.AND.YY(II).EQ.0.0) THEN
        WRITE (11,*) '   '
        II= II-1
        T=0.5*COEFFA*XX(II)**2 + COEFFB*XX(II) + COEFFC
        DIFF= T-YY(II)
        ZZ(II)= T
        WRITE (11,31) YY(II), ZZ(II), XX(II), 10**(XX(II)), INDICAT(II)
        GO TO 520
       ELSE IF (XX(II).EQ.-99.AND.YY(II).EQ.-99) THEN
        GO TO 530
       ELSE
        ZZ(II)= DIFF + YY(II)
        WRITE (11,31) YY(II), ZZ(II), XX(II), 10**(XX(II)), INDICAT(II)
        GO TO 520
      END IF
  530 CONTINUE
      WRITE (10,*) '----------------------------------------------------
     $----------------'
C
  600 CONTINUE

      write (*,*) ' '
      write (*,*) 'If you have executed this program simply to obtain'
      write (*,*) 'a median recession index (for use in program RORA)'
      write (*,*) 'then you might elect not to write additional data '
      write (*,*) 'describing the master recession curve to file     '
      write (*,*) 'RECSUM.txt: .............                         '
      write (*,*) '                                                  '
      write (*,*) '  To only write the median recession index (to file'
      write (*,*) '     "index.txt"), enter 1 '
      write (*,*) '  To write more results, including information about'
      write (*,*) '    the master recession curve to file "recsum.txt" '
      write (*,*) '    (in addition to "index.txt"), enter 2 '
      read (*,*) numxx
      
      if (numxx.ne.1) then

      WRITE (12,17) INFILE,SEASON,IYEARST+IBEFORE,IYEAREN+IBEFORE,         C
     $ NMRECES, KMIN,KMED,KMAX,MNLOGQC,MXLOGQC,0.5*COEFFA,
     $ COEFFB,COEFFC

       write (*,*) ' '
       write (*,*) 'Results have been written to "recsum.txt"  '

      endif

      write (13,13) INFILE, KMED                                           C

      write (*,*) ' '
      write (*,*) 'Results have been written to "index.txt"  '

  700 continue

      CLOSE (10,STATUS='KEEP')
      CLOSE (11,STATUS='KEEP')
      CLOSE (12,STATUS='KEEP')
      close (13,status='keep')

      WRITE (*,*) ' '
      WRITE (*,*) ' Click the "enter" key to terminate. '
      READ (*,'(A)') YESNO

      STOP
      END




C --------- THIS SUBROUTINE MAKES TABULAR OUTPUT OF RECESSION DATA: -----

      SUBROUTINE TABLE(QLOG,FLOW,IYR,IMO,IDA,IPEAK,IMIN,IMAX,PICK,
     $                 IDISPLA)
      REAL FLOW(60), QLOG(60), DELQLOG(60)
      INTEGER IYR(60), IMO(60), IDA(60)
      CHARACTER*80 PICK
      IF (PICK.EQ.'t') THEN
         ISTART=1
        ELSE
   10     CONTINUE
          WRITE (*,*) 'ENTER STARTING DAY (1, 11, OR 21)'
          READ (*,'(A)') PICK
          IF(PICK.EQ.'1') THEN
             ISTART=1
            ELSEIF(PICK.EQ.'11') THEN
             ISTART=11
            ELSEIF(PICK.EQ.'21') THEN
             ISTART=21
            ELSE
             WRITE (*,*) 'OPTION NOT RECOGNIZED'
             GO TO 10
           END IF
        END IF
      IEND= ISTART+IDISPLA-1
      IF(ISTART.GT.60) ISTART=60
      IF(IEND.GT.60) IEND= 60
   20 FORMAT (1I6, 3F10.4, 1I8, 3I8)
      DO 40 IT= ISTART, IEND
          IF(IT.EQ.1) THEN
              DELQLOG(IT)= 999.0
            ELSE
              DELQLOG(IT)= QLOG(IT)-QLOG(IT-1)
           END IF
   40 CONTINUE
      WRITE(*,*) 'TIME AFTER         DELTA             TIME AFTER'
      WRITE(*,*) '   PEAK   LOG Q    LOG Q      Q        START   YEAR  .
     $  MONTH    DAY'
      DO 230 IT= ISTART,IEND
         IF (IT.GT.IMAX.OR.IT.LT.IMIN) THEN
            WRITE (*,*) '---'
          ELSEIF (QLOG(IT).EQ.-99.9) THEN
            WRITE (*,*) '---'
          ELSEIF (QLOG(IT).EQ.-88.8) THEN
            WRITE (*,*) 'STREAMFLOW = ZERO '
          ELSE
           WRITE (*,20) IT, QLOG(IT), DELQLOG(IT), FLOW(IT), IT+IPEAK,
     $                       IYR(IT), IMO(IT), IDA(IT)
         END IF
  230           CONTINUE
      RETURN
      END
C
C
C
C ------ THIS SUBROUTINE MAKES GRAPHICAL OUTPUT OF RECESSION DATA: ----
C
      SUBROUTINE GRAPH (QLOG,FLOW,IYR,IMO,IDA,IPEAK,IMIN,IMAX,
     $              XLGQMAX,XLGQMIN,PICK,IDISPLA)
      REAL FLOW(60), QLOG(60)
      INTEGER IYR(60), IMO(60), IDA(60)
      CHARACTER*1 BLANK(80)
      CHARACTER*80 PICK
      DO 2 I=1,80
    2 BLANK(I)= ' '
      IF (PICK.EQ.'g') THEN
           ISTART=1
           INTERVL=1
        ELSE
    3      CONTINUE
           WRITE (*,*) 'ENTER STARTING DAY (1, 11, OR 21)'
           READ(*,'(A)') PICK
            IF(PICK.EQ.'1') THEN
                ISTART= 1
              ELSEIF(PICK.EQ.'11') THEN
                ISTART=11
              ELSEIF(PICK.EQ.'21') THEN
                ISTART=21
              ELSE
                WRITE (*,*) 'OPTION NOT RECOGNIZED'
                GO TO 3
             END IF
    4      CONTINUE
           WRITE (*,*) 'ENTER TIME INTERVAL (1=EVERY DAY, 2=EVERY OTHER
     $ DAY)'
           READ (*,'(A)') PICK
             IF(PICK.EQ.'1') THEN
                 INTERVL=1
               ELSEIF(PICK.EQ.'2') THEN
                 INTERVL=2
               ELSE
                 WRITE (*,*) 'OPTION NOT RECOGNIZED'
                 GO TO 4
               END IF
         END IF
      IEND= ISTART+(IDISPLA-1)*INTERVL
        IF(ISTART.GT.60) ISTART=60
        IF(IEND.GT.60) IEND=60
        IIEND= ISTART
    8   IIEND= IIEND + INTERVL
        IF(IIEND.LT.60.AND.IIEND.LT.IEND) GO TO 8
        IF(IIEND.GT.IEND) THEN
            IEND= IIEND - INTERVL
          ELSE
            IEND= IIEND
         END IF
        WRITE (*,9) ' LOG Q =', XLGQMIN, XLGQMAX
    9 FORMAT (A8, 6X, 1F6.2, 55X, 1F6.2)
   10 FORMAT (1I3, 1I5, 2I3, 5X, A50)
   12 FORMAT (1I3, 1I5, 2I3, 5X, 60A1)
      DO 80 IT= ISTART, IEND, INTERVL
         IF (IT.GT.IMAX.OR.IT.LT.IMIN) THEN
              WRITE (*,10) IT, IYR(IT), IMO(IT), IDA(IT),
     $             '---'
           ELSEIF(QLOG(IT).EQ.-99.9) THEN
              WRITE (*,10) IT, IYR(IT), IMO(IT), IDA(IT),
     $             '---'
           ELSEIF (QLOG(IT).EQ.-88.8) THEN
              WRITE (*,10) IT, IYR(IT), IMO(IT), IDA(IT), 
     $             'STREAMFLOW = ZERO '
           ELSEIF(QLOG(IT).GT.XLGQMAX.OR.QLOG(IT).LT.XLGQMIN) THEN
              WRITE (*,10) IT, IYR(IT), IMO(IT), IDA(IT),
     $             ':::::'
           ELSE
              DIFF= QLOG(IT) - XLGQMIN
              NUMBLNK= INT(DIFF*60/(XLGQMAX-XLGQMIN))
              WRITE (*,12) IT, IYR(IT), IMO(IT), IDA(IT),
     $                 (BLANK(J), J=1,NUMBLNK), '*'
          END IF
   80   CONTINUE
      RETURN
      END




C -----THIS SUBROUTINE PERFORMS LEAST-SQUARES REGRESSION TO FIND BEST-FIT ---
C ---------------- EQUATION OF LINEAR BASIS ( Y = A*X + B ) -----------------
C
      SUBROUTINE REGRES2(X,Y,NUM,COEFFA,COEFFB)
      REAL A(2,4)
      REAL X(50), Y(50)
      DO 20 I=1,2
      DO 20 J=1,4
   20 A(I,J)= 0.0
      RT1= 0.0
      RT2= 0.0
      DO 40 I=1,NUM
      A(1,1)= A(1,1) + X(I)**2
      A(1,2)= A(1,2) + X(I)
      A(2,1)= A(2,1) + X(I)
      RT1= RT1 + X(I)*Y(I)
   40 RT2= RT2 + Y(I)
      A(2,2)= REAL(NUM)
      N=2

      CALL INVERS(A,N)

      COEFFA= A(1,1)*RT1 + A(1,2)*RT2
      COEFFB= A(2,1)*RT1 + A(2,2)*RT2

      RETURN
      END
C
C -----  THIS SUBROUTINE CHANGES AN N*N MATRIX [A] TO ITS INVERSE -----
C ---------  (Note: The matrix is actually N*(2N) internally) ---------
C
      SUBROUTINE INVERS(A,N)
      REAL A(2,4)
      DO 10 I=1,N
      DO 10 J=N+1,2*N
   10 A(I,J)= 0.0
      DO 20 I= 1,N
   20 A(I,N+I) = 1.0
      DO 90 K=1,N
          DO 40 I=1,N
              TEMP=A(I,K)
                  DO 30 J=1,2*N
                  A(I,J)=A(I,J)/TEMP
   30             CONTINUE
   40     CONTINUE
          DO 80 I=1,N
              IF (I.NE.K) THEN
                  DO 70 J=1,2*N
                     A(I,J)=A(I,J)-A(K,J)
   70             CONTINUE
              END IF
   80 CONTINUE
   90 CONTINUE
      DO 150 I=1,N
           AP=A(I,I)
               DO 130 J=1,2*N
                   A(I,J)=A(I,J)/AP
  130           CONTINUE
  150 CONTINUE
      DO 180 I=1,N
      DO 180 J=1,N
  180 A(I,J)=A(I,J+N)
      RETURN
      END



      SUBROUTINE ORDER(M,LIST1,LIST2,LIST3)
C  INPUT IS 3 LISTS OF NUMBERS. ALL LISTS (LIST1,LIST2,AND LIST3) HAVE
C  M NUMBERS. OUTPUT IS SAME, IN ASCENDING ORDER, SORTED BY VALUES
C  IN LIST1. NOTE: LIST3 IS MADE UP OF INTEGERS.
      INTEGER I,M,NUM,PASSES
      REAL A
      REAL LIST1(50)
      REAL LIST2(50)
      INTEGER LIST3(50)
      CHARACTER SWAPED*3
      SWAPED= 'YES'
      NUM= M
      PASSES= 0
   10 CONTINUE
         SWAPED= 'NO'
         PASSES= PASSES + 1
         I=1
   20    CONTINUE
            IF(LIST1(I).LE.LIST1(I+1)) THEN
               ELSE
                 A= LIST1(I+1)
                 LIST1(I+1) = LIST1(I)
                 LIST1(I)= A
                 B= LIST2(I+1)
                 LIST2(I+1)= LIST2(I)
                 LIST2(I)= B
                 C= LIST3(I+1)
                 LIST3(I+1)= LIST3(I)
                 LIST3(I)= C
                 SWAPED= 'YES'
            ENDIF
            I=I+1
         IF (I.LE.(NUM-PASSES)) THEN
           GOTO 20
           END IF
      IF (SWAPED.EQ.'YES') THEN
        GOTO 10
        END IF
      RETURN
      END

