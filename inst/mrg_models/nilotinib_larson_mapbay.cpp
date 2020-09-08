$PROB

- drug: Nilotinib
- model_ref: Larson et al, Eur Clin Pharmacol, 2012


$PARAM @annotated
TVCL : 21   : Clearance (L/h)
TVV1 : 58   : Central volume (L)
TVV2 : 181  : Perif volume (L)
Q    : 66.5 : Intercompartmental cleareance (L/h)
ALAG : 0.67 : Lag time (h)
D    : 9.88 : Infusion duration (h)

tF1 : 1 : Fixed bioavailability ()
tDOSE : 0.843 : Dose effect on F1 ()
tSEX : 0.903 : Sex effect on F1 ()

tCL_TBIL : -0.256  : Bil effect on CL ()
tCL_ASAT : -0.0277 : ASAT effect on CL ()

ETA1 : 0 : CL (L/h)
ETA2 : 0 : V1 (L)
ETA3 : 0 : V2 (L)

$PARAM @covariates @annotated

AOLA : 300 : Amount of Last Administration (mg)
SEX  : 0   : SEX (0=M, 1=F)
TBIL : 0.5 : Total Bilirubin (n*ULN)
ASAT : 0.5 : ASAT (n*ULN)

$OMEGA @block
0.141
0.163 0.627 
0.000 0.000 0.978

$SIGMA 0.088804 0.012100

$CMT @annotated
CENT : Central compartment (ng/mL) [ADM, OBS]
PER : Perif compartment

$GLOBAL
#define DV (CENT/V1 + CENT*EPS(1)/V1 + EPS(2)) 
double K10, K12, K21, F1, CL, V1, V2, bDOSE;

$MAIN

if (AOLA < 400) bDOSE = 0 ;
else bDOSE = 1 ;

F1 = tF1 * pow (tDOSE , bDOSE) * pow (tSEX , 1-SEX) ;
CL = TVCL * exp (tCL_TBIL * (TBIL - 0.5)) * exp (tCL_ASAT * (ASAT - 0.5)) * exp(ETA1 + ETA(1)) ;
V1 = TVV1 * exp(ETA2 + ETA(2)) ;
V2 = TVV2 * exp(ETA3 + ETA(3)) ;

D_CENT = D ;
ALAG_CENT = ALAG ;
F_CENT = F1 ;

K10 = CL/V1 ;
K12 = Q/V1 ;
K21 = Q/V2 ;

$ODE

dxdt_CENT = - K10*CENT - K12*CENT + K21*PER ;
dxdt_PER = K12*CENT - K21*PER ;

$CAPTURE @annotated
DV : Plasma concentration (ng/mL)
ETA1 : CL (L/h)
ETA2 : V1 (L)
ETA3 : V2 (L)