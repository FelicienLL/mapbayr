$PROB 

- drug: Cabozantinib
- model_ref: Lacy et al, Cancer Chemother Pharmacol, 2018

$PARAM @annotated
TVKA     : 0.804   : h-1
D2       : 2.435   : h
TVCL     : 2.457   : L.h-1
TVVC     : 157.178 : L
Q      : 30.154  : L.h-1
VP     : 188.666 : L
ALAG1  : 0.789   : h
TVFR     : 0.847   : 
DOSEKA : 0.566   : 

ETA1 : 0 : KA (h-1)
ETA2 : 0 : CL (L/h)
ETA3 : 0 : VC (L)
ETA4 : 0 : FR ()

$PARAM @annotated @covariates

AOLA   : 40 : Amount of last administration (mg)

$OMEGA 2.063 0.202 0.233 0.466
$SIGMA 0.000 0.118 

$CMT @annotated
DEPOT   : Depot      () [ADM]
CENTRAL : Central    (mg/L) [ADM, OBS]
PERIPH  : Peripheral ()

$GLOBAL
double K12, K23, K32, K20, KA, CL, VC, PHI, FR   ;

$TABLE
double DV  = (CENTRAL / VC) * exp( EPS(2) ) ;
double ET1 = ETA(1) ;
double ET2 = ETA(2) ;
double ET3 = ETA(3) ;
double ET4 = ETA(4) ;

$MAIN

KA = TVKA * pow(AOLA,DOSEKA) * exp(ETA1 + ETA(1)) ;
CL = TVCL * exp(ETA2 + ETA(2))  ;
VC = TVVC * exp(ETA3 + ETA(3))  ;  
FR= exp(log(TVFR/(1-TVFR))+(ETA4 + ETA(4)))/(1+exp(log(TVFR/(1-TVFR))+(ETA4 + ETA(4)))) ;

K12 = KA ; 
K23 = Q  / VC                  ;
K32 = Q  / VP                  ;
K20 = CL / VC                  ;

F_DEPOT    = FR           ;
ALAG_DEPOT = ALAG1             ;

F_CENTRAL  = 1 - FR            ;
D_CENTRAL  = D2             ;
ALAG_CENTRAL = 0.000001             ;

$ODE
dxdt_DEPOT   = - K12 * DEPOT                                          ; 
dxdt_CENTRAL =   K12 * DEPOT   + K32 * PERIPH - (K23 + K20) * CENTRAL ; 
dxdt_PERIPH  =   K23 * CENTRAL - K32 * PERIPH                         ;

$CAPTURE @annotated
DV : mg/L
ETA1 : KA (h-1)
ETA2 : CL (L/h)
ETA3 : VC (L)
ETA4 : FR ()