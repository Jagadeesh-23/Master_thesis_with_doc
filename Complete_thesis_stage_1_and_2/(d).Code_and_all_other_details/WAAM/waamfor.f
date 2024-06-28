
      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C

	 DIMENSION FLUX(2), TIME(2), COORDS(3)
	 CHARACTER*80 SNAME
	 B = 1.67
	 C = 1.67
	 AR = 3.34
	 AF = 1.002
	 FF = 0.46
	 PI = 3.14
	 X = COORDS(1)
	 Y = COORDS(2)
	 Z = COORDS(3)
	 T = TIME(2)
	 VEL = 10.0
	 XC = 5.0+(VEL*T)
	 YC = 5.0
	 ZC = 6.0
	 U = 15.3
	 ETA = 0.9
	 I = 120.0
	 PI = 3.141593
	 IF ((X - XC) >= 0.0) THEN
		Rtermf1 = (((X - XC)**2.0)/(AF**2.0)) 
		Rtermf2 = (((Y - YC)**2.0)/(B**2.0))
		Rtermf3 = (((Z - ZC)**2.0)/(C**2.0))
		Rtermf= Rtermf1 +Rtermf2 +Rtermf3
		Qtermf= 507.6441933* (EXP(-3.0*Rtermf))
		flux(1) = Qtermf
	 END IF
	 IF ((X - XC) < 0.0) THEN
		FR = 2.0 - FF
		Rtermr1 = (((X - XC)**2)/(AR**2)) 
		Rtermr2 = (((Y - YC)**2)/(B**2))
		Rtermr3 = (((Z - ZC)**2)/(C**2))
		Rtermr= Rtermr1+ Rtermr2 + Rtermr3
		Qtermr= 509.851342* (EXP(-3.0* Rtermr))
		flux(1) = Qtermr
	 END IF

	 RETURN
	 END