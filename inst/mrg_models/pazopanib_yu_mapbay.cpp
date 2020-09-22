$PROB

- drug: Pazopanib
- model_ref: Yu et al, Clin Pharmacokinet, 2017

$PARAM @annotated
TVCL     : 0.27  : 1  Apparent clearance (L.h-1)
TVV2     : 2.43  : 2  Central volume (L)
TVKAF    : 0.40  : 3  Fast absorption rate constant (h-1)
TVKAS    : 0.12  : 4  Slow absorption rate constant (h-1)
TVALAG3  : 0.98  : 5  Lag time (h)
TVQ24    : 0.99  : 6  Intercompartmental clearance (L.h-1)
TVV4     : 25.1  : 7  Peripheral volume (L)
TVLAMBDA : 0.15  : 8  First-order decay constant (day-1)
TVDCRP   : 0.501 : 9  Magnitude of decrease constant ()
TVFR     : 0.361 : 10 Fraction of fast absorption ()
TVED50   : 480   : 11 (mg)

ETA1 : 0 : CL  (L/h)
ETA2 : 0 : KAF (h-1)
ETA3 : 0 : V4  (L)
ETA4 : 0 : rF  ()
ETA5 : 0 : rF_IOV1 ()
ETA6 : 0 : rF_IOV2 ()

$PARAM @covariates @annotated

CYCLE : 1 : default cycle for computation of IOV
AOLA  : 200 : Amount Of Last Administration
TOLA  : 0 : Time Of Last Administration

$OMEGA
0.095481
1.96    
0.964324
0.126736
0.555025
0.555025

$SIGMA
0.0064
9.61  

$CMT @annotated
DEPOT1     : Depot compartment 1    () [ADM]       
CENTRAL    : Central compartment (mg/L)[OBS]  
DEPOT3     : Depot compartment 3    () [ADM]   
PERIPHERAL : Peripheral compartment () []

$GLOBAL
double CL, V2, KAF, KAS, ALAG3, Q24, V4, LAMBDA, DCRP, ED50, TVFT, TVFD, TVFI, TVF, FR, F1, F3, K20, K24, K42 ;

$TABLE
double DV  = (CENTRAL / V2) * (1 + EPS(1)) + EPS(2) ;

$MAIN
CL = TVCL * exp(ETA1 + ETA(1))    ;
V2 = TVV2                      ;
KAF= TVKAF* exp(ETA2 + ETA(2))    ;
KAS= TVKAS                     ;
ALAG3 = TVALAG3                ;
Q24 = TVQ24                    ;
V4 = TVV4 * exp(ETA3 + ETA(3))    ;
LAMBDA = TVLAMBDA / 24         ;
DCRP = TVDCRP                  ;
ED50 = TVED50                  ;


TVFT = 1 - DCRP + DCRP * exp(-LAMBDA * TOLA)          ;
TVFD = 1 - (1*(AOLA - 200) / (ED50 + (AOLA - 200))) ;

TVFI = TVFD * TVFT * exp(ETA4 + ETA(4)) ;

if (CYCLE == 1){
    TVF = TVFI * exp(ETA5 + ETA(5))   ;
} else {
    TVF = TVFI * exp(ETA6 + ETA(6))   ;
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
ALAG_DEPOT1 = 0.000001 ;

$ODE

dxdt_DEPOT1     = -KAF*DEPOT1 ;
dxdt_CENTRAL    = KAF*DEPOT1 + KAS*DEPOT3 - K20*CENTRAL - K24*CENTRAL + K42*PERIPHERAL;
dxdt_DEPOT3     = -KAS*DEPOT3;
dxdt_PERIPHERAL = K24*CENTRAL - K42 * PERIPHERAL;

$CAPTURE @annotated
DV : Pazopanib concentration central (mg/L)
ETA1: CL
ETA2: KA
ETA3: V
ETA4: rF
ETA5: iov1
ETA6: iov2
