$PROB Reference model 

$PARAM @annotated
TVCL   : 4.00 : Clearance (L/h)
TVVC   : 70.0 : Central volume of distribution (L)
TVKA   : 1.00 : Absorption rate (h-1)
TVCLmet : 2.5 : Clerance metabolite (L/h)
TVVCmet : 60 : Central volume metabolite (L)

ETA1 : 0 : CL
ETA2 : 0 : VC
ETA3 : 0 : KA
ETA4 : 0 : CLmet
ETA5 : 0 : VCmet

$OMEGA
0.2 // CL
0.2 // VC
0.2 // KA 
0.2 // CLmet
0.2 // VCmet

$SIGMA 
0.05 // err prop parent
0   //  err additive parent
0.05 // err prop metabolite
0   //  err additive metabolite

$CMT @annotated
DEPOT   : Depot () [ADM]
CENTRAL : Central () [OBS]
CENTRALMET : Central metabolite () [OBS]

$TABLE
double PAR = (CENTRAL / VC) * (1 + EPS(1)) + EPS(2) ;
double MET = (CENTRALMET / VCmet) * (1 + EPS(3)) + EPS(4) ;
double DV = PAR ;
if(self.cmt == 3) DV = MET ;

$MAIN
double CL     = TVCL     * exp(ETA(1) + ETA1 ) ; 
double VC     = TVVC     * exp(ETA(2) + ETA2 ) ;
double KA     = TVKA     * exp(ETA(3) + ETA3 ) ;
double CLmet  = TVCLmet  * exp(ETA(4) + ETA4 ) ; 
double VCmet  = TVVCmet  * exp(ETA(5) + ETA5 ) ;

double K23 = CL / VC ;
double K30 = CLmet / VCmet ; 

$ODE
dxdt_DEPOT      = - KA * DEPOT ;
dxdt_CENTRAL    = - K23 * CENTRAL + KA * DEPOT ;
dxdt_CENTRALMET = 0.7 * K23 * CENTRAL - K30 * CENTRALMET ;

$CAPTURE DV PAR MET