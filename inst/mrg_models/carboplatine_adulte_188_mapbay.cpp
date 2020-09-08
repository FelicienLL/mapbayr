$PROB

- drug: Carboplatine
- model_ref: Non publie (188 patients adultes)

$PARAM @annotated
TVCL : 6.38  : 1  Clearance (L.h-1)
TVVC : 9.50  : 2  Central volume (L)
TVVP : 9.56  : 3  Peripheral volume (L)
TVQ  : 2.70  : 4  Intercompartmental clearance (L.h-1)

ETA1 : 0 : CL (L/h)
ETA2 : 0 : VC (L)
ETA3 : 0 : VP (L)

$PARAM @annotated @covariates
BSA : 1.73 : Body surface area (m2)

$OMEGA 0.1050 0.0378 0.0260
$SIGMA 0.0381, 0

$CMT @annotated
CENTRAL : Central compartment (mg/L) [ADM, OBS]
PERIPHERAL : Peripheral compartment ()

$GLOBAL
double CL, VC, Q, VP, D1, K12, K21, K10 ;

$TABLE
double DV  = (CENTRAL / VC)*(1+EPS(1)) ;


$MAIN
CL = TVCL * exp(ETA1 + ETA(1))      ;
VC = TVVC * exp(ETA2 + ETA(2)) * BSA     ;
VP = TVVP * exp(ETA3 + ETA(3))      ;
Q  = TVQ       ;

K12 = Q /VC ;
K21 = Q /VP ;
K10 = CL/VC ;

$ODE

dxdt_CENTRAL    = - K10*CENTRAL - K12*CENTRAL + K21*PERIPHERAL;
dxdt_PERIPHERAL =   K12*CENTRAL - K21 * PERIPHERAL;

$CAPTURE @annotated
DV : Carboplatin concentration central (mg/L)
ETA1 : CL (L/h)
ETA2 : VC (L)
ETA3 : VP (L)
