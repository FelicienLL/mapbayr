$PROB

- drug: Nilotinib
- model_name: Nilotinib Giles
- model_ref: Giles et al, Eur Clin Pharmacol, 2013
- error_model: Prop + Add (mg/L)
- adm_cmt: 1
- obs_cmt: 1
- concentration_unit: ng/mL

$PARAM @annotated
TVCL : 12.8  : Clearance (L/h)
TVV1 : 56.0  : Central volume (L)
TVV2 : 247   : Perif volume (L)
Q    : 103   : Intercompartmental clearance (L/h)
ALAG : 0.746 : Lag time (h)
D    : 3.02  : Infusion duration (h)

TVF1  : 1     : Fixed bioavailability
tDOSE : -1.65 : Dose effect on F1
tSEX  : 0.835 : Sex effect on F1
tMD   : 0.649 : Ratio of F1 (morning/evening dose)

tCL_TBIL : -0.117 : 

ETA1 : 0 : F1 ()
ETA2 : 0 : CL (L/h)
ETA3 : 0 : V1 (L)
ETA4 : 0 : V2 (L)

$PARAM @annotated @covariates

AOLA  : 50  : Amount of Last Administration (mg)
SEX   : 1   : Sex (0=M, 1=F)
MATIN : 0   : Last intake in the morning ? (0=no, 1=yes)
TBIL  : 0.5 : Total Bilirubin (n*ULN)


$OMEGA @block
0.3250
0.0660  0.0722
0.0928 -0.0277  0.9790
0.0000  0.0000  0.0000 0.42

$SIGMA 0.11088900 0.00395641

$CMT @annotated
CENT : Central compartment
PER : Perif compartment

$GLOBAL
double K10, K12, K21, CL, F1, V1, V2 ;
#define DV (CENT/V1 + CENT*EPS(1)/V1 + EPS(2))

$MAIN
F1 = TVF1 * exp (tDOSE * (AOLA - 50) / 1150)  * pow (tMD , MATIN) * pow (tSEX , 1-SEX) * exp (ETA1 + ETA(1));
CL = TVCL * exp (tCL_TBIL * (TBIL - 0.5)) * exp (ETA2 + ETA(2)) ;
V1 = TVV1 * exp (ETA3 + ETA(3)) ;
V2 = TVV2 * exp (ETA4 + ETA(4)) ;

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
DV : Plasma concentration (mass/volume)
CL : 
V1 : 
V2 : 
F1 :
ETA1 : eta F1
ETA2 : eta CL
ETA3 : eta V1
ETA4 : eta V2
