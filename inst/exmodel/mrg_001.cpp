$PROB Reference model 

$PARAM @annotated
TVCL   : 4.00 : Clearance (L/h)
TVVC   : 70.0 : Central volume of distribution (L)
TVKA   : 1.00 : Absorption rate (h-1)

ETA1 : 0 : CL
ETA2 : 0 : VC
ETA3 : 0 : KA

$OMEGA
0.2 // CL
0.2 // VC
0.2 // KA 

$SIGMA 
0.05 // err prop
0   //  err additive 


$CMT @annotated
DEPOT   : Depot () [ADM]
CENTRAL : Central () [OBS]

$TABLE
double DV = (CENTRAL / VC) * (1 + EPS(1)) + EPS(2) ;

$MAIN
double CL  = TVCL  * exp(ETA(1) + ETA1 ) ; 
double VC  = TVVC  * exp(ETA(2) + ETA2 ) ;
double KA  = TVKA  * exp(ETA(3) + ETA3 ) ;

double K20 = CL / VC ;

$ODE
dxdt_DEPOT   = - KA * DEPOT ;
dxdt_CENTRAL = - K20 * CENTRAL + KA * DEPOT ;

$CAPTURE DV