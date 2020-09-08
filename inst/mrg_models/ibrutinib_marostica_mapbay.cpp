$PROB

- drug: Ibrutinib
- model_ref: Marostica et al, Cancer Chemoter Pharmacol, 2015

$PARAM @annotated
TVALAG    : 0.283 : Lag time (h)
KA        : 0.463 : Absorption constant (h-1)
TVQ       : 865   : Intercompartmental clearance (L/h)
TVCL      : 1060  : Elimination clearance (L/h)
TVV2      : 246   : Central volume (L)
TVV3      : 9620  : Peripheral volume (L)

tF1fast  : 0.666 : Bioavailability if fast
tF1other : 1     : Bioavailability if mod fast or fed

tD1fed   : 3.29  : Infusion duration (h) if fed
tD1other : 1.1   : Infusion duration (h) if fast or mod fast

mWT      : 80.4  : Median weight (kg)
coeffWT  : 0.641 : Power on volumes (allometric coefficient) for body weight

tAA      : 1.61  : Antacids factor on D1

ETA1 : 0 : CL (L/h)
ETA2 : 0 : V2 (L)
ETA3 : 0 : Q (L/h)
ETA4 : 0 : V3 (L)
ETA5 : 0 : ALAG (h)
ETA6 : 0 : D1 (h)
ETA7 : 0 : F1 ()

$PARAM @covariates @annotated

WT : 82.7 : Weight (kg)
AA : 0    : Prise d'antiacides (non = 0, oui = 1)
MC : 2    : Meal conditions (1 = overnight fasted, 2 = mod fast (30 minutes avant à 2h après le repas), 3 = fed)

$OMEGA
0.047961 
2.3409 
0.368449  
0.223729 
0.077284 
0.043681 
0.394384


$SIGMA 
0.528529
0

$CMT @annotated

DEPOT : Extravascular compartment () [ADM]
CENT : Central compartment (ng/mL)  [OBS]
PER : Periph compartment

$GLOBAL
#define DV (CENT/V2 + CENT*EPS(1)/V2)
double K20, K23, K32, V2, V3, D1, F1, TVF1, TVD1, CL, Q, ALAG ;


$MAIN

D_DEPOT = D1 ;
ALAG_DEPOT = ALAG ;
F_DEPOT = F1 ;

if ( MC == 1 ) TVD1 = tD1other , TVF1 = tF1fast ;
if ( MC == 2 ) TVD1 = tD1other , TVF1 = tF1other ;
if ( MC == 3 ) TVD1 = tD1fed , TVF1 = tF1other ;

CL = TVCL * exp (ETA(1) + ETA1) ;
Q = TVQ * exp (ETA(3) + ETA3) ;
ALAG = TVALAG * exp (ETA(5) + ETA5) ;
D1 = TVD1 * pow(tAA, AA) * exp (ETA(6) + ETA6) ;
F1 = TVF1 * exp (ETA(7) + ETA7) ;

V2 = pow(WT/mWT, coeffWT) * TVV2 * exp (ETA(2) + ETA2) ;
V3 = pow(WT/mWT, coeffWT) * TVV3 * exp (ETA(4) + ETA4) ;

K20 = CL/V2 ;
K23 = Q/V2 ;
K32 = Q/V3 ;


$ODE
dxdt_DEPOT = - KA*DEPOT ;
dxdt_CENT = KA*DEPOT - K23*CENT + K32*PER - K20*CENT ;
dxdt_PER = K23*CENT - K32*PER ;

$CAPTURE @annotated
DV : Plasma concentration (mass/volume) 
CL :
V2 : 
V3 : 
F1 : 
ETA1 : eta CL
ETA2 : eta V2
ETA3 : eta Q
ETA4 : eta V3
ETA5 : eta ALAG
ETA6 : eta D1
ETA7 : eta F1


  
  