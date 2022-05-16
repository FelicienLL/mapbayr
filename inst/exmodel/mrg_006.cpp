$PROB LAG model 

$PARAM @annotated
TVCL   : 4.00 : Clearance (L/h)
TVVC   : 70.0 : Central volume of distribution (L)
TVKA   : 1.00 : Absorption rate (h-1)
TVD2   : 4.00 : Zero-order constant (h)
FR     : 0.2  : Fraction absorbed from Depot 1 ()

ETA1 : 0 : CL
ETA2 : 0 : VC
ETA3 : 0 : KA
ETA4 : 0 : D2

$OMEGA
0.2 // CL
0.2 // VC
0.2 // KA 
0.2 // D2

$SIGMA 
0.05 // err prop
0   //  err additive 


$CMT @annotated
DEPOT1   : Depot () [ADM]
CENTRAL : Central () [ADM, OBS]

$TABLE
double DV = (CENTRAL / VC) * (1 + EPS(1)) + EPS(2) ;

$MAIN
double CL  = TVCL  * exp(ETA(1) + ETA1 ) ; 
double VC  = TVVC  * exp(ETA(2) + ETA2 ) ;
double KA  = TVKA  * exp(ETA(3) + ETA3 ) ;
double D2  = TVD2  * exp(ETA(4) + ETA4 ) ;
double K20 = CL / VC ;

F_DEPOT1  = FR ;
F_CENTRAL = 1 - FR ; 
D_CENTRAL = D2 ;

$ODE
dxdt_DEPOT1   = - KA * DEPOT1 ;
dxdt_CENTRAL  = - K20 * CENTRAL + KA * DEPOT1 ;

$CAPTURE DV