$PROB
- drug: Examplinib3
- model_ref: XXX, J Pharmacokinet, 2020

$PARAM @annotated
TVCL   : .7 : Clearance (volume/time)
TVV1   : 20 : Central volume (volume)
TVV2   : 10 : Peripheral volume of distribution (volume)
Q      :  3 : Inter-compartmental clearance (volume/time)

ETA1 : 0 : Clearance (L/h)
ETA2 : 0 : Central volume (L)
ETA3 : 0 : Peripheral volume (L)

$OMEGA 0.3 0.2 0.1
$SIGMA 0.06 0

$CMT @annotated
CENT   : Central compartment (mg/L)[ADM, OBS]
PERIPH : Peripheral compartment ()

$TABLE
double DV = (CENT/V2) *(1 + EPS(1)) ;

$MAIN
double CL = TVCL * exp(ETA1 + ETA(1)) ;
double V1 = TVV1 * exp(ETA2 + ETA(2)) ;
double V2 = TVV2 * exp(ETA3 + ETA(3)) ;

$PKMODEL ncmt = 2

$CAPTURE @annotated
DV : Plasma concentration (mass/time)

