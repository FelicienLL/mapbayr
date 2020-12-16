$PROB

- drug: Examplinib1
- model_ref: XXX et al, J Pharmacokinet, 2020

$PARAM @annotated
TVKA  : 1   : Absorption constant (h-1)
D2    : 3   : Duration of absorption (h)
TVCL  : 3   : Clearance (L.h-1)
TVVC  : 200 : Central volume (L)
Q     : 30  : Intercompartmental clearance (L.h-1)
VP    : 10  : Peripheral volume (L)
ALAG1 : 1.5 : Lag time (h)
FR    : 0.5 : Fraction absorbed from DEPOT ()
CL_WT : 0.2 : Effect of weight on CL ()

ETA1 : 0 : KA (h-1)
ETA2 : 0 : CL (L/h)
ETA3 : 0 : VC

$PARAM @annotated @covariates
CYCLE: 1  : Cycle ()
AOLA : 60 : Amount of last admin (mg)
TOLA : 0  : Time of last admin (mg)
WT   : 80 : Body weight (kg)

$OMEGA @block
0.15
0.05 0.20
0.00 0.05 0.25

$SIGMA
0.000 // prop
0.100 // (log) additive

$CMT @annotated
DEPOT   : Depot      ()  [ADM]
CENTRAL : Central    (mg/L)[ADM, OBS]
PERIPH  : Peripheral ()  []

$TABLE
double DV  = (CENTRAL / VC) * exp(EPS(2)) ;

$MAIN
double KA = TVKA * exp(ETA1 + ETA(1)) ;
double CL = TVCL * exp(ETA2 + ETA(2)) * pow(WT/80, CL_WT) ;
double VC = TVVC * exp(ETA3 + ETA(3)) ;

double K12 = KA      ;
double K23 = Q  / VC ;
double K32 = Q  / VP ;
double K20 = CL / VC ;

F_DEPOT    = FR  ;
ALAG_DEPOT = ALAG1 ;

F_CENTRAL  = 1 - FR ;
D_CENTRAL  = D2 ;
ALAG_CENTRAL = 0.000001 ;

$ODE
dxdt_DEPOT   = - K12 * DEPOT                                          ;
dxdt_CENTRAL =   K12 * DEPOT   + K32 * PERIPH - (K23 + K20) * CENTRAL ;
dxdt_PERIPH  =   K23 * CENTRAL - K32 * PERIPH                         ;

$CAPTURE DV
