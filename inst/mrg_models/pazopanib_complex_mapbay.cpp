$PROB 

- drug: Pazopanib
- model_name: Pazopanib Complex
- model_ref: Mourey et al, ..., ... 
- error_model: Prop
- adm_cmt: 1, 3
- obs_cmt: 2
- concentration_unit: mg/L

$PARAM @annotated
TVCL     : 0.45600181 : 1  Apparent clearance (L.h-1)
TVV2     : 4.13905790 : 2  Central volume (L)
TVKAF    : 0.96478395 : 3  Fast absorption rate constant (h-1)
TVKAS    : 0.19681617 : 4  Slow absorption rate constant (h-1)
TVALAG3  : 0.90639081 : 5  Lag time (h)
TVQ24    : 2.27600511 : 6  Intercompartmental clearance (L.h-1)
TVV4     : 22.13634530 : 7  Peripheral volume (L)
TVLAMBDA : 0.08327196 : 8  First-order decay constant (day-1)
TVDCRP   : 0.54044185 : 9  Magnitude of decrease constant (%)
TVFR     : 0.28189122 : 10 Fraction of fast absorption (%)

ETA1 : 0 : CL (L/h)
ETA2 : 0 : KAF (h-1)
ETA3 : 0 : V4 (L)
ETA4 : 0 : rF ()
ETA5 : 0 : rF_IOV1 ()
ETA6 : 0 : rF_IOV2 ()

$PARAM @covariates @annotated

CYCLE : 1 : Cycle ()
TOLA  : 0 : Time of Last Administration (h)

$OMEGA 0.2201188 3.9105070 0.2580377 0.1367867 0.1790653 0.1790653
$SIGMA 0.03240458 0.00000000

$CMT @annotated
DEPOT1 : Depot compartment 1 (mcg)
CENTRAL : Central compartment (mcg)
DEPOT3 : Depot compartment 3 (mcg)
PERIPHERAL : Peripheral compartment (mcg)

$GLOBAL
double CL, V2, KAF, KAS, ALAG3, Q24, V4, LAMBDA, DCRP, TVFT, TVFI, TVF, FR, F1, F3, K20, K24, K42 ;

$TABLE
double DV  = (CENTRAL / V2)*(1+EPS(1)) ;
double ET1 = ETA(1) ;
double ET2 = ETA(2) ;
double ET3 = ETA(3) ;
double ET4 = ETA(4) ;
double ET5 = ETA(5) ;
double ET6 = ETA(6) ;

$MAIN
CL = TVCL * exp(ETA1 + ETA(1)) ;
V2 = TVV2             ;
KAF= TVKAF* exp(ETA2 + ETA(2)) ;
KAS= TVKAS             ;
ALAG3 = TVALAG3         ;
Q24 = TVQ24             ;
V4 = TVV4 * exp(ETA3 + ETA(3))    ;
LAMBDA = TVLAMBDA / 24   ;
DCRP = TVDCRP             ;

TVFT = 1 - DCRP + DCRP * exp(-LAMBDA*TOLA) ;

TVFI = TVFT * exp(ETA4 + ETA(4)) ;

if (CYCLE == 1){
    TVF = TVFI * exp(ETA5 + ETA(5));
} else {
    TVF = TVFI * exp(ETA6 + ETA(6));
}

FR = TVFR ;
F1 = TVF * FR ;
F3 = TVF * (1 - FR) ;

K20 = CL / V2 ;
K24 = Q24 / V2 ;
K42 = Q24 / V4 ;

F_DEPOT1 = F1 ;
F_DEPOT3 = F3 ;
ALAG_DEPOT3 = ALAG3 ;

$ODE

dxdt_DEPOT1     = -KAF*DEPOT1 ; 
dxdt_CENTRAL    = KAF*DEPOT1 + KAS*DEPOT3 - K20*CENTRAL - K24*CENTRAL + K42*PERIPHERAL;
dxdt_DEPOT3     = -KAS*DEPOT3; 
dxdt_PERIPHERAL = K24*CENTRAL - K42 * PERIPHERAL;

$CAPTURE @annotated
DV : Pazopanib concentration central (mg/L)
ETA1 : CL (L/h)
ETA2 : KAF (h-1)
ETA3 : V (L)
ETA4 : rF ()
ETA5 : rF_IOV1 ()
ETA6 : rF_IOV2 ()