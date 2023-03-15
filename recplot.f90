PROGRAM RECPLOT
USE WINTERACTER
INTEGER, PARAMETER  :: IDR_MENU1  = 30001
INTEGER, PARAMETER  :: ID_OPEN    = 40001
INTEGER, PARAMETER  :: ID_EXIT    = 40002
TYPE(WIN_STYLE)     :: WINDOW
TYPE(WIN_MESSAGE)   :: MESSAGE
INTEGER             :: ITYPE, NVALS, I
CHARACTER(LEN=255)  :: FNAME
REAL                   X(60)
REAL                   Y(60)
CALL WInitialise(' ')              ! Initialise Winteracter
WINDOW%FLAGS  = SysMenuOn + MinButton + MaxButton
WINDOW%X      = 700                ! Horiz position?
WINDOW%Y      = 100                ! Vertical position?
WINDOW%WIDTH  = 500                ! window size
WINDOW%HEIGHT = 500                !
WINDOW%MENUID = IDR_MENU1          ! Menu identifier
WINDOW%TITLE  = 'RECPLOT'          ! Root window title
CALL WindowOpen(WINDOW)            ! Open root window
FNAME  = 'recdata.txt'
NVALS = 0
! Read and plot data file "recdata.txt"......
OPEN(20,FILE=FNAME,STATUS='OLD')
 READ(20,*)  NVALS
   DO I = 1,NVALS
     READ (20,*) X(I), Y(I)
   END DO
CLOSE(20)
CALL DrawGraph(X,Y,NVALS)

! Allow user to plot other data files ......
DO                                 ! Loop until user terminates
  CALL WMessage(ITYPE, MESSAGE)
  SELECT CASE (ITYPE)
    CASE (MenuSelect)              ! Menu item selected
      SELECT CASE (MESSAGE%VALUE1)
        CASE (ID_OPEN)             ! Select file to plot
          CALL WSelectFile('Data file|*.dat|',PromptOn,FNAME,'Load Data')
          IF (WInfoDialog(ExitButtonCommon).EQ.CommonOpen) THEN
               OPEN(20,FILE=FNAME,STATUS='OLD')
               READ(20,*)  NVALS
                DO I = 1,NVALS
                   READ (20,*) X(I), Y(I)
                END DO
              CLOSE(20)
              CALL DrawGraph(X,Y,NVALS)
          END IF
        CASE (ID_EXIT)             ! Exit program (menu option)
          EXIT
      END SELECT
    CASE (Expose,Resize)           ! Need to redraw picture
      CALL DrawGraph(X,Y,NVALS)
    CASE (CloseRequest)            ! Exit program (e.g. Alt/F4)
      EXIT
  END SELECT
END DO
CALL WindowClose()                 ! Remove program window
STOP
END PROGRAM RECPLOT

SUBROUTINE DrawGraph(X,Y,NVALS)  ! Draw graph
USE  WINTERACTER
REAL                 X(60)
REAL                 Y(60)
CHARACTER (LEN=3) :: STR
REAL              :: XMIN,XMAX,YMIN,YMAX,XLEN,YLEN,XPOS
INTEGER           :: IX,ISTART

!    Calculate axes parameters
       XMIN = 0.0
OPEN (22,FILE="ranges.txt", STATUS='OLD')
  READ (22,*)
  READ (22,*) XMAX
  READ (22,*) YMIN
  READ (22,*) YMAX
 CLOSE (22)
XLEN = XMAX
YLEN = YMAX - YMIN
CALL IGrUnits(XMIN-0.1*XLEN,YMIN-0.1*YLEN, &
              XMAX+0.1*XLEN,YMAX+0.1*YLEN)
!  Draw simple axes
CALL IGrMoveTo(XMIN,YMAX)
CALL IGrLineTo(XMIN,YMIN)
CALL IGrLineTo(XMAX,YMIN)

!  Draw line graph
CALL IGrMoveTo(X(1),Y(1))
DO IX = 2,NVALS
  CALL IGrLineTo(X(IX),Y(IX))
END DO

! Add annotation to X axis
IXMAX=INT(XMAX)
CALL IGrCharJustify('C')
DO IX = 0,IXMAX,5
  CALL IGrMoveTo(REAL(IX),YMIN)
  CALL IGrLineTo(REAL(IX),YMIN-YLEN*0.025)
  CALL IntegerToString(IX,STR,'(I3)')
  ISTART = ILocateChar(STR)
  CALL IGrCharOut(REAL(IX),YMIN-YLEN*0.06,STR(ISTART:))
END DO

! Add annotation to Y axis
IYMAX=INT(YMAX)
CALL IGrCharJustify('C')
DO IX = 0,IYMAX,1
  CALL IGrMoveTo(XMIN,REAL(IX))
  CALL IGrLineTo(XMIN-XLEN*0.025,REAL(IX))
  CALL IntegerToString(IX,STR,'(I3)')
  ISTART = ILocateChar(STR)
  CALL IGrCharOut(XMIN-XLEN*0.06,REAL(IX),STR(ISTART:))
END DO

RETURN
END SUBROUTINE DrawGraph