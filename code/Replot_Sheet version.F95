WINAPP 
!--------------------SHEET1_DATA_TO_PLOT-------
MODULE SHEET1_DATA_TO_PLOT
   USE MSWIN
   PARAMETER (NARR1=2000)! it limit array size
   INTEGER, PARAMETER :: RKIND=SELECTED_REAL_KIND(15,307)
   INTEGER::SHEET1_P5
   REAL (KIND=RKIND):: SHEET1_P1,SHEET1_P2,SHEET1_P3,SHEET1_P4=NARR1
   REAL (KIND=RKIND):: SHEET1_YARR1(NARR1)
   CONTAINS
   
!----------------------------------------------  
   SUBROUTINE SHEET1_PREPARE_DATA
   INTEGER :: I,SHEET1_XX
   SHEET1_XX=0
   SHEET1_P5=SHEET1_P4
!---------------SHEET1_YARR1 INITIAL-----------
	 DO I=1,NARR1
     	SHEET1_YARR1(I)=0
     END DO
!----------------------------------------------
   IF (SHEET1_P4 .EQ. NARR1) then
     
     DO I=1,NARR1
        SHEET1_YARR1(I)=SHEET1_P1*SIN(SHEET1_XX/SHEET1_P3)*EXP(-SHEET1_XX/SHEET1_P2)
        SHEET1_XX=SHEET1_XX+1
     ENDDO       
   ELSE
     DO I=1,SHEET1_P4
        SHEET1_YARR1(I)=SHEET1_P1*SIN(SHEET1_XX/SHEET1_P3)*EXP(-SHEET1_XX/SHEET1_P2)
        SHEET1_XX=SHEET1_XX+1
     ENDDO
   END IF
   END SUBROUTINE SHEET1_PREPARE_DATA
!----------------------------------------------
   INTEGER FUNCTION SHEET1_RE_PLOT()
      CALL SHEET1_PREPARE_DATA
      CALL SIMPLEPLOT_REDRAW@
      SHEET1_RE_PLOT=2
   END FUNCTION SHEET1_RE_PLOT 

   INTEGER FUNCTION SHEET1_OUTPUT_DATA()
      OPEN(unit=10,file='sheet1_output.dat',status='unknown')
      WRITE(10,*) "      P1     ","    P2      ","     P3        ","X     ","Y"
      DO I=1,SHEET1_P4
        WRITE(10,20) SHEET1_P1,SHEET1_P2,SHEET1_P3,I,SHEET1_YARR1(I)
        20 format (F10.2,1X,F10.2,1X,F10.2,1X,I5,F10.5)
      ENDDO
      I=WINIO@('Data Ouput Complete!%TA%BT[OK]')
      SHEET1_OUTPUT_DATA=2
      CLOSE(10)
   END FUNCTION SHEET1_OUTPUT_DATA

END MODULE SHEET1_DATA_TO_PLOT
!--------------------SHEET1_DATA_TO_PLOT-------


!--------------------SHEET2_DATA_TO_PLOT-------
MODULE SHEET2_DATA_TO_PLOT
   USE MSWIN
   PARAMETER (NARR2=2000)! it limit array size
   INTEGER, PARAMETER :: RKIND=SELECTED_REAL_KIND(15,307)
   INTEGER::SHEET2_P5
   REAL (KIND=RKIND):: SHEET2_P1,SHEET2_P2,SHEET2_P3,SHEET2_P4=NARR2
   REAL (KIND=RKIND):: SHEET2_P6,SHEET2_P7,SHEET2_P8
   REAL (KIND=RKIND):: SHEET2_YARR2(NARR2)
   REAL (KIND=RKIND):: SHEET2_YARR3(NARR2)
   CONTAINS
   
!----------------------------------------------  
   SUBROUTINE SHEET2_PREPARE_DATA
   INTEGER :: I,SHEET2_XX
   SHEET2_XX=0
   SHEET2_P5=SHEET2_P4
!---------------SHEET2_YARR2,3 INITIAL---------
	 DO I=1,NARR2
     	SHEET2_YARR2(I)=0
        SHEET2_YARR3(I)=0
     END DO
!----------------------------------------------
   IF (SHEET2_P4 .EQ. NARR2) then
     
     DO I=1,NARR2
        SHEET2_YARR2(I)=SHEET2_P1*SIN(SHEET2_XX/SHEET2_P3)*EXP(-SHEET2_XX/SHEET2_P2)
        SHEET2_YARR3(I)=SHEET2_P6*SIN(SHEET2_XX/SHEET2_P8)*EXP(-SHEET2_XX/SHEET2_P7)
        SHEET2_XX=SHEET2_XX+1
     ENDDO       
   ELSE
     DO I=1,SHEET2_P4
        SHEET2_YARR2(I)=SHEET2_P1*SIN(SHEET2_XX/SHEET2_P3)*EXP(-SHEET2_XX/SHEET2_P2)
        SHEET2_YARR3(I)=SHEET2_P6*SIN(SHEET2_XX/SHEET2_P8)*EXP(-SHEET2_XX/SHEET2_P7)
        SHEET2_XX=SHEET2_XX+1
     ENDDO
   END IF
   END SUBROUTINE SHEET2_PREPARE_DATA
!----------------------------------------------
   INTEGER FUNCTION SHEET2_RE_PLOT()
      CALL SHEET2_PREPARE_DATA
      CALL SIMPLEPLOT_REDRAW@
      SHEET2_RE_PLOT=2
   END FUNCTION SHEET2_RE_PLOT 

   INTEGER FUNCTION SHEET2_OUTPUT_DATA()
      OPEN(unit=20,file='sheet2_output(Line1).dat',status='unknown')
      OPEN(unit=30,file='sheet2_output(Line2).dat',status='unknown')
      WRITE(20,*) "      P1     ","    P2      ","     P3        ","X     ","Y"
      WRITE(30,*) "      P1     ","    P2      ","     P3        ","X     ","Y"
      DO I=1,SHEET2_P4
        WRITE(20,20) SHEET2_P1,SHEET2_P2,SHEET2_P3,I,SHEET2_YARR2(I)
        20 format (F10.2,1X,F10.2,1X,F10.2,1X,I5,F10.5)
        WRITE(30,30) SHEET2_P6,SHEET2_P7,SHEET2_P8,I,SHEET2_YARR3(I)
        30 format (F10.2,1X,F10.2,1X,F10.2,1X,I5,F10.5)
      ENDDO
      I=WINIO@('Data Ouput Complete!%TA%BT[OK]')
      SHEET2_OUTPUT_DATA=2
      CLOSE(20)
      CLOSE(30)
   END FUNCTION SHEET2_OUTPUT_DATA

END MODULE SHEET2_DATA_TO_PLOT
!--------------------SHEET2_DATA_TO_PLOT-----------------




!--------------------APS_TOP----------------------------- 
MODULE APS_SHEET
   IMPLICIT NONE
   INTEGER ::PTR
CONTAINS

   INTEGER FUNCTION CBUPDATE()
      USE MSWIN
      CALL SEE_PROPERTYSHEET_PAGE@(PTR) 
      CBUPDATE=2
   END FUNCTION CBUPDATE
END APS_SHEET
!--------------------APS_TOP----------------------------- 

!--------------------ABOUT------------------------------- 
MODULE ABOUT_MODULE
   USE MSWIN	
   IMPLICIT NONE
   INTEGER::I
   INTEGER::PARAM_WINDOW
CONTAINS

   INTEGER FUNCTION ABOUT()
   I=WINIO@('%CA[ABOUT Guo WEI]&')
   I=WINIO@('%FN[TIMES NEW ROMAN]%TS%BF%CNGUO-WEI PLOT&',2.0D0)
   I=WINIO@('%TS%4NL&',1.0D0)
   I=WINIO@('%CNPROGRAM WRITTEN IN 2018/01/20%2NL&')
   I=WINIO@('%TS%TC%CN%BFGUO-WEI&',1.5D0,RGB@(255,0,0))
   I=WINIO@('%2NL%CN%9`BT[OK]')
   ABOUT=1
   END FUNCTION ABOUT
END ABOUT_MODULE
!--------------------ABOUT-------------------------------



PROGRAM GUO_WEI_PLOT_PROGRAM
   USE MSWIN
   USE ABOUT_MODULE
   USE APS_SHEET!sheet use
   USE SHEET1_DATA_TO_PLOT
   USE SHEET2_DATA_TO_PLOT
   IMPLICIT NONE
   INTEGER :: PS1,PS2,PS3 !sheet use
   PS1=1;PS2=2;PS3=3 !sheet use
   SHEET1_P1=1.5;SHEET1_P2=150.0;SHEET1_P3=15;SHEET1_P5=10 !initial value
   SHEET2_P1=1.5;SHEET2_P2=150.0;SHEET2_P3=15;SHEET2_P5=10 !initial value
   SHEET2_P6=1.5;SHEET2_P7=150.0;SHEET2_P8=15
   PARAM_WINDOW=0
   CALL SHEET1_PREPARE_DATA
   CALL SHEET2_PREPARE_DATA
!-----------------sheet 1--------------------------------           
   I=WINIO@('%CA[ Single Function]&')!sheet use     
   I=WINIO@('%WW[TOPMOST]&')
   I=WINIO@('%SY[3D]%SF&') 
   I=WINIO@('%CN%eq[Y=P{sub 1}Sin(X/P{sub 3}) e{sup -X/P{sub 2}}]%ff%nl&',350,55)
   I=WINIO@('%OB[SCORED]%bg[white]&') !%OB IS OPEN A BOX
   I=WINIO@('%CNP%SD1%`SD%FL%10RF%2NL&',0.01D0,1.0D3,SHEET1_P1) !%SD TO PROVIDE SUBSCRIPT TEXT
   I=WINIO@('%CNP%SD2%`SD%FL%10RF%2NL&',0.01D0,1.0D3,SHEET1_P2) !%FL SPECIFY THE MIN & MAX %RF FORMATS.
   I=WINIO@('%CNP%SD3%`SD%FL%10RF%2NL&',0.01D0,1.0D3,SHEET1_P3) !10RF IS BOX LENGTH,NL IS UP AND DOWN INTERVAL  
   I=WINIO@('%CNDATA_NUMBER%FL%10RF%2NL&',1D0,2.0D3,SHEET1_P4) !SHEET1_P4 is real type,because WINIO use
   														!1D0=1,2.0D3=2000,limit input
   I=WINIO@('%CN%^BT[REPLOT]&',SHEET1_RE_PLOT) !%CN IS IN THE MIDDLE
   I=WINIO@('%^BT[OUTPUT]%2NL&',SHEET1_OUTPUT_DATA) 
   I=WINIO@('%CB&') !%CB IS CLOSE A BOX
   I=WINIO@('%CN%PL[X_AXIS="TIME(MILISECONDS)",Y_AXIS=AMPLITUDE,'&      
        //'TITLE="PLOT",COLOUR=RED]&',&
         400,300,SHEET1_P5,0.0D0,1.0D0,SHEET1_YARR1)  !SHEET1_P5 is integer type,SHEET1_P5=SHEET1_P4,only change type 	 
         								!SHEET1_YARR1 limit is 2000,only change SHEET1_P5 data number 
   I=WINIO@('%PV&')
   I=WINIO@('%SH',PS1)!sheet use
!-----------------sheet 1-------------------------------- 

!-----------------sheet 2--------------------------------   
   I=WINIO@('%CA[ Double Function]&')!sheet use     
   I=WINIO@('%WW[TOPMOST]&')
   I=WINIO@('%SY[3D]%SF&')  
   I=WINIO@('%CN%eq[Y=P{sub 1}Sin(X/P{sub 3}) e{sup -X/P{sub 2}}]%ff%nl&',350,55)
   I=WINIO@('%OB[SCORED]%bg[white]&')
   
   I=WINIO@('%CNLine1                Line2%NL&')  
   I=WINIO@('%CNP%SD1%`SD%FL%8RF&',0.01D0,1.0D3,SHEET2_P1)
   I=WINIO@('P%SD1%`SD%FL%8RF%2NL&',0.01D0,1.0D3,SHEET2_P6)
   
   I=WINIO@('%CNP%SD2%`SD%FL%8RF&',0.01D0,1.0D3,SHEET2_P2)
   I=WINIO@('P%SD2%`SD%FL%8RF%2NL&',0.01D0,1.0D3,SHEET2_P7)
   
   I=WINIO@('%CNP%SD3%`SD%FL%8RF&',0.01D0,1.0D3,SHEET2_P3)
   I=WINIO@('P%SD3%`SD%FL%8RF%2NL&',0.01D0,1.0D3,SHEET2_P8)

   
   I=WINIO@('%CNDATA_NUMBER%FL%10RF%2NL&',1D0,2.0D3,SHEET2_P4)
   I=WINIO@('%CN%^BT[REPLOT]&',SHEET2_RE_PLOT)
   I=WINIO@('%^BT[OUTPUT]%2NL&',SHEET2_OUTPUT_DATA) 
   I=WINIO@('%CB&')  
   I=WINIO@('%CN%PL[X_AXIS="TIME(MILISECONDS)",Y_AXIS=AMPLITUDE,'&      
        //'TITLE="PLOT",N_GRAPHS=2,COLOUR=RED,COLOUR=BLUE]&',&
         400,300,SHEET2_P5,0.0D0,1.0D0,SHEET2_YARR2,SHEET2_YARR3)
   I=WINIO@('%PV&')
   I=WINIO@('%SH',PS2)!sheet use
!-----------------sheet 2--------------------------------


!-----------------sheet 3--------------------------------       
   I=WINIO@('%CA[ SHEET 3]&')!sheet use
   
   I=WINIO@('SHEET THREE %BT[HELLOW ZZZ WORLD]%SH',PS3)!sheet use
!-----------------sheet 3-------------------------------- 

!-----------------Main program use-----------------------
   I=WINIO@('%CA[GUO-WEI PLOT PROGRAM]&')
   I=WINIO@('%MI[ICON_1]&')
   !I=WINIO@('%WW[NO_BORDER]&')
   I=WINIO@('%MN[&FILE[E&XIT]]&','EXIT')
   I=WINIO@('%MN[&HELP[&ABOUT GREEN WEI]]&',ABOUT)
   I=WINIO@('%OB[RAISED]&')!sheet use
   I=WINIO@('%`3PS&',PS1,PS2,PS3,PTR) !sheet use
   I=WINIO@('%CB')!sheet use
!-----------------Main program use-----------------------
END PROGRAM GUO_WEI_PLOT_PROGRAM


RESOURCES
   
ICON_1 ICON SALFLOGO.ICO  



