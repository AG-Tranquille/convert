      PROGRAM SCREEN
C          BY AL RUTLEDGE, USGS           2005 VERSION
C  THIS PROGRAM READS A DATA FILE OF DAILY STREAMFLOW THEN
C  GENERATES A GRAPHIC SHOWING COMPLETENESS OF THE RECORD.
C  THIS GRAPHIC IS THE SAME AS THE GRAPHIC THAT IS GENERATED
C  BY PART, RORA, PREP, AND RECESS, EXCEPT THIS PROGRAM PLACES
C  THE GRAPHIC ON A FILE INSTEAD OF THE COMPUTER MONITOR.
C
C
C  SEVERAL DECLARATION STATEMENTS PERTAIN TO ARRAY SIZES:
C          MAXIMUM NUMBER OF YEARS = 120.
C          MAXIMUM NUMBER OF DAYS = 44000.
C              (ALSO NOTE INITIALIZING STATEMENT AFTER LABEL 10)
C          RETRIEVE DAILY VALUES AFTER THE YEAR 1890.
      COMMON/BIG/Q(120,12,31)

      CHARACTER*1 FLAG(120,12)
      REAL Q
      CHARACTER*16 INFILE                                                  C
      CHARACTER*80 YESNO
      CHARACTER*15 STANUM                                                  C
      CHARACTER*9 LINE
      CHARACTER*5 AGENCY                                                   C
      CHARACTER*180 BUF_LINE                                               C
      REAL FLONUM

      IBEFORE=1890


      OPEN (UNIT=16,FILE='screenout.txt')



C     NOTE: SOME COMPILERS MIGHT REQUIRE A LINE HERE "BACKSPACE 10"
C             THIS SHOULD NOT BE DONE IF RUNNING ON A DG.

      BACKSPACE 10

    5 FORMAT (2I6,3F11.2,1F8.2)

C
C-------------------------- INITIALIZE VARIABLES : -------------------
C 
      DO 10 IYEAR=1,120
      DO 10 IMONTH=1,12
      DO 10 IDAY=1,31
   10 Q(IYEAR,IMONTH,IDAY)=-999.0


   14 FORMAT (F8.0)
   15 FORMAT (1I5,3F10.2,5X,3I4)
   16 FORMAT (A12,1X,1F6.2,2x,1I4,'-',1I4,4F8.2,
     $ 1F6.1)
   17 FORMAT (A16, 1F8.0)
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
C
C -------------- READ THE DV FILE OF STREAMFLOW: ----------------------
C
      WRITE (*,*) 'GIVE THE NAME OF THE STREAMFLOW DAILY-VALUES  '           C
      WRITE (*,*) 'FILE   (the program is case-sensitive)        '           C
      write (*,*) '(Example file that is included: "Indian.txt") '           C
      READ (*,'(A)') INFILE

      WRITE (16,*) 'THIS IS OUTPUT OF THE SCREEN PROGRAM,        '           2007
      WRITE (16,*) 'VERSION OF JANUARY 2007                      '           2007
      WRITE (16,*) '   '                                                     2007
      WRITE (16,*) 'READING FILE NAMED ',INFILE
      OPEN (UNIT=9, FILE=INFILE)
      IYEAR=1
   30 READ (9,21) LINE
      IF( LINE.NE.'agency') THEN
            GO TO 30
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
      WRITE (16,*) 'FIRST YEAR IN RECORD = ', IFRSTYR
      WRITE (16,*) ' LAST YEAR IN RECORD = ', ILSTYR

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

      WRITE (16,*) '                 MONTH  '
      WRITE (16,*) ' YEAR   J F M A M J J A S O N D'

      IFRSTYR= IFRSTYR-IBEFORE
      ILSTYR= ILSTYR-IBEFORE

      do 72 IYEAR=IFRSTYR, ILSTYR

           DO 55 IMONTH=1,12
   55      FLAG(IYEAR,IMONTH)='.'
           DO 70 IMONTH=1,12
             DO 60 IDAY=1,31
               IF(Q(IYEAR,IMONTH,IDAY).EQ.-999) FLAG(IYEAR,IMONTH)='X'
               IF(Q(IYEAR,IMONTH,IDAY).EQ.-99) FLAG(IYEAR,IMONTH)='X'
   60         CONTINUE
   70         CONTINUE
            WRITE (16,73) IYEAR+IBEFORE,(FLAG(IYEAR,IMONTH),IMONTH=1,12)

   72 continue

        WRITE (16,*) ' '
        WRITE (16,*) ' COMPLETE RECORD = .      INCOMPLETE = X  '
        WRITE (16,*) ' '
   73 FORMAT (1I6, 2X, 12A2)

      write (*,*) 'Output is written to file "screenout.txt" '

      write (*,*) ' '
      write (*,*) ' Click the "enter" key to terminate. '
      read (*,'(A)') YESNO

      CLOSE (9,STATUS='KEEP')
      CLOSE (16,STATUS='KEEP')


      STOP
      END




