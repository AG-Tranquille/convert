      PROGRAM CURV
C  BY AL RUTLEDGE, USGS.   2007 version.
C  This is an auxiliary program provided with the "recess" 
C  program (see USGS WRIR 98-4148).  The 2005 version allows
c  for changes in file-naming and file construction of the
c  output file of the recess program (file recsum.txt).
c
c  This program reads recsum.txt, then derives data pairs of
c  time and log of flow (LogQ), based on variables in recsum.txt.
c  These variables include coefficients of a polynomial
c  expression -- A, B, and C (wrir 98-4148, equation 5), and
c  limits of LogQ as per the following --
c     (1) The maximum LogQ from all recession segments
c         analyzed using recess.
c     (2) The minimum LogQ from all recession segments
c         analyzed using recess.
c  This program also reads drainage area from "station.txt".
c
c  Output, which is written to "curvout.txt", includes time,
c  LogQ, and a number of other modes for expressing flow.
c  The file curvout.txt can be used with separate graphics
c  to display the Master Recession Curve. 


      CHARACTER*16 INFILE
      CHARACTER*16 FNAME
      CHARACTER*1 SEASON
      CHARACTER*1 SEAS
      CHARACTER*9 LINE
      CHARACTER*80 YESNO
      REAL A,B,C,MAXLOGQ,MINLOGQ,MINQ,DELLOGQ,LOGQ,T
      REAL KMIN,KMED,KMAX
      OPEN (UNIT=10,FILE='curvout.txt')
   15 FORMAT (5F15.5)
   17 FORMAT (A12,A1,1X,1I4,1X,1I4,1I3,3F6.1,2F8.3,1F9.4,2F10.4)
   19 FORMAT (A12,1F8.0)
   21 FORMAT (A9)
   23 FORMAT (1F12.0)
      WRITE (*,*) 'THIS PROGRAM READS ONE OF THE OUTPUT FILES OF '
      WRITE (*,*) 'PROGRAM "RECESS" (FILE "recsum.txt") TO OBTAIN VARI-'
      WRITE (*,*) 'ABLES TO CONSTRUCT THE MASTER RECESSION CURVE'
      WRITE (*,*) 'FOR STATIONS DESIGNATED BY THE USER.  THE PROGRAM'
      WRITE (*,*) 'WILL SCAN recsum.txt TO FIND THE STREAMFLOW DATA-'
      WRITE (*,*) 'FILE NAME AND SEASON DESIGNATION.  (NOTE -- THE '
      WRITE (*,*) 'SEASON IS DESIGNATED BY THE USER WHEN EXECUTING '
      WRITE (*,*) 'RECESS.) '
      WRITE (*,*) ' '
 
      write (10,*) 'T is time in days.'
      write (10,*) 'LogQcfs is the log of flow in CFS. '
      write (10,*) 'LogQcfsmi is the log of flow in CFS per sq mile. '
      write (10,*) 'Qcfs is the flow in CFS. '
      write (10,*) 'Qcfsmi is the flow in CFS per square mile. '
      write (10,*) '-------------------------------------------------'
      write (10,*) ' '
      WRITE (10,*) '         T            LogQcfs       LogQcfsmi      .
     $ Qcfs           Qcfsmi'

   25 CONTINUE
C
C
C ----------------ENTER SOME USER OPTIONS: ---------------------------------
C
      WRITE (*,*) 'GIVE THE NAME OF THE STREAMFLOW DATA FILE '
      WRITE (*,*) '(FOR EXAMPLE, Indian.txt )..... '
      READ (*,'(A)') INFILE
      WRITE (*,*) 'ENTER SEASON OF INTEREST (w, v, s, f, or n)'
      READ (*,'(A)') SEASON
      WRITE (10,*) ' '
      WRITE (10,*) INFILE, SEASON
C
C ------------- READ LIST FILE GIVING STATION PROPERTIES:  -------------------
C                    OBTAIN DRAINAGE AREA(DA)
C
      WRITE (*,*) 'READING FILE NAMED station.txt'
      OPEN (UNIT=11,FILE='station.txt')
      DO 30 J=1,10
         READ (11,21) LINE
   30 CONTINUE
   32 READ(11,19) FNAME,DA
      IF(FNAME.NE.INFILE) GO TO 32
      WRITE (*,*) 'NAME OF STREAMFLOW DATA FILE ... ', FNAME
      WRITE (*,*) 'DRAINAGE AREA:', DA
      write (*,*) ' '
      CLOSE (11,STATUS='KEEP')
C
C -------READ recsum.txt TO OBTAIN VARIABLES TO DEFINE THE MRC: -------------
C
      WRITE (*,*) 'READING recsum.txt'
      OPEN (UNIT=8,FILE='recsum.txt')
      DO 35 J=1,22
         READ (8,21) LINE
   35 CONTINUE
   40 READ (8,17) FNAME,SEAS,IYEARST,IYEAREN,NMRECES,
     $      KMIN,KMED,KMAX,MINLOGQ,MAXLOGQ,A,B,C
        IF(FNAME.NE.INFILE) GO TO 40
        IF(SEAS.NE.SEASON) GO TO 40
      WRITE (*,*) 'COEFFICIENTS OF THE MASTER RECESSION CURVE '
      WRITE (*,*) 'ARE AS FOLLOWS ....... '
      WRITE (*,*) '     A           B           C   '
      WRITE (*,*) A,B,C
      CLOSE (8,STATUS='KEEP')
C
C --------- WRITE X AND Y COMPONENTS OF THE MRC TO FILE curvout.txt: ------
C
      DELLOGQ= (MAXLOGQ-MINLOGQ) / 49.0
      LOGQ= MAXLOGQ
      DO 100 I=1,50
          T= A*(LOGQ**2) + B*LOGQ + C
          WRITE (10,15) T,LOGQ, (LOG((10.0**LOGQ)/DA))/2.3025851, 
     $        10.0**LOGQ, (10.0**LOGQ)/DA
          LOGQ=LOGQ-DELLOGQ
  100 CONTINUE
C
C
      WRITE (*,*) ' '
      WRITE (*,*) 'DO YOU WANT TO MAKE ANOTHER MRC? (y OR n) '
      READ (*,'(A)') YESNO
      IF (YESNO.NE.'n') GO TO 25
      WRITE (*,*) ' '
      WRITE (*,*) 'OUTPUT IS WRITTEN TO FILE "curvout.txt". '
      WRITE (*,*) 'AFTER TERMINATING THIS PROGRAM, VIEW THE '
      WRITE (*,*) 'MASTER RECESSION CURVE USING A GRAPHICS '
      WRITE (*,*) 'APPLICATION THAT WILL READ "curvout.txt". '
      CLOSE (10,STATUS='KEEP')

      write (*,*) ' '
      write (*,*) 'Click the "enter" key to terminate.'
      read (*,'(A)') YESNO

      STOP
      END
