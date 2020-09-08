$PROB 

- drug: Pazopanib
- model_ref: Le Louedec et al, These d'exercice DES PH, 2019 

$PARAM @annotated
TVKA    : 1.44319  : Absorption rate constant (h-1)
TVCL    : 0.960487 : Clearance (L.h-1)
TVV     : 32.4405  : Volume (L)
TVALAG1 : 0.464736 : Lag time (h)

ETA1 : 0 : KA (h-1)
ETA2 : 0 : CL (L/h)
ETA3 : 0 : V (L)
ETA4 : 0 : ALAG1 (h)
ETA5 : 0 : CL_IOV1 ()
ETA6 : 0 : CL_IOV2 ()
ETA7 : 0 : V_IOV1 ()
ETA8 : 0 : V_IOV2 ()

$PARAM  @covariates @annotated

CYCLE : 1 : default cycle for computation of IOV

$OMEGA @block

0.547456 
0.000000 0.267111
0.000000 0.306600 0.571839
0.000000 0.000000 0.000000 0.212423
0.000000 0.000000 0.000000 0.000000 0.231702
0.000000 0.000000 0.000000 0.000000 0.000000 0.231702
0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.157868
0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.157868

$SIGMA 0.04445646 0.00000000

$CMT @annotated
DEPOT1 : Depot compartment 1 () [ADM]
CENTRAL : Central compartment (mg/L) [OBS]

$GLOBAL
double CL, V, ALAG1, KA, K20 ;

$TABLE
double DV  = (CENTRAL / V)*(1+EPS(1)) ;
double ET1 = ETA(1) ;
double ET2 = ETA(2) ;
double ET3 = ETA(3) ;
double ET4 = ETA(4) ;
double ET5 = ETA(5) ;
double ET6 = ETA(6) ;
double ET7 = ETA(7) ;
double ET8 = ETA(8) ;


$MAIN
KA    = TVKA    * exp(ETA1 + ETA(1)) ;
ALAG1 = TVALAG1 * exp(ETA4 + ETA(4)) ;

if (CYCLE == 1){
    CL    = TVCL    * exp(ETA2 + ETA(2) + ETA5 + ETA(5)) ;
    V     = TVV     * exp(ETA3 + ETA(3) + ETA7 + ETA(7)) ;
} else {
    CL    = TVCL    * exp(ETA2 + ETA(2) + ETA6 + ETA(6)) ;
    V     = TVV     * exp(ETA3 + ETA(3) + ETA8 + ETA(8)) ;
}

K20 = CL / V ;
ALAG_DEPOT1 = ALAG1 ;

$ODE

dxdt_DEPOT1  = -KA*DEPOT1 ; 
dxdt_CENTRAL = KA*DEPOT1 - K20*CENTRAL ;

$CAPTURE @annotated
DV : Pazopanib concentration central (mcg/L)
ETA1 : KA (h-1)
ETA2 : CL (L/h)
ETA3 : V (L)
ETA4 : ALAG1 (h)
ETA5 : CL_IOV1 ()
ETA6 : CL_IOV2 ()
ETA7 : V_IOV1 ()
ETA8 : V_IOV2 ()