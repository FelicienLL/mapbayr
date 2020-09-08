$PROB

- drug: Examplinib
- model_ref: Modelisator et al, Clin Pharmacokinet, 2020
- error_model: Prop

$PARAM @annotated
TVALAG   : 0.238192 : Lag time (h)
TVD      : 0.988671 : Infusion duration (h)
KAibru   : 1.55664  : Absorption constant for ibrutinib (h-1)
TVKAdhd  : 1.20812  : Absorption constant for DHD (h-1)

Qibru    : 171.094 : Intercompartmental clearance for ibrutinib (L/h)
TVCLmet  : 149.622 : Metabolisation clearance of ibrutinib (L/h)
TVCLibru : 241.899 : Elimination clearance of ibrutinib (L/h)
Qdhd     : 50.3012 : Intercompartmental clearance for DHD (L/h)
TVCLdhd  : 180.972 : Elimination clearance of DHD (L/h)

TVV2     : 1005.03 : Central volume (L)
TVV3     : 1481.01 : Peripheral volume (L)

ETA1 : 0 : V3 (L)
ETA2 : 0 : V2 (L)
ETA3 : 0 : CLdhd (L/h)
ETA4 : 0 : CLibru (L/h)
ETA5 : 0 : KAdhd (h-1)
ETA6 : 0 : CLmet (L/h)
ETA7 : 0 : ALAG (h)
ETA8 : 0 : D (h)
ETA9 : 0 : IOV1_CLibru ()
ETA10: 0 : IOV2_CLibru ()
ETA11: 0 : IOV1_CLdhd  ()
ETA12: 0 : IOV2_CLdhd  ()

$PARAM @covariates @annotated
CYCLE : 1 : Cycle (n)

$OMEGA @block
0.465173
0.432131 0.512330000
0.220876 0.217668000 0.228809
0.374779 0.421162000 0.195424 0.3658180
0.298891 0.287158000 0.156798 0.2487940 0.345464
0.000000 0.000591386 0.180014 0.0217671 0.129532 0.347017000
$OMEGA @block
0.501473 
0.282336 0.844851
$OMEGA
0.197216
0.197216
0.0637684
0.0637684

$SIGMA
0.128496
0
0.0639887
0

$CMT @annotated
EV       : Extravascular compartment         ()[ADM]
CENTibru : Central compartment for ibrutinib (pg/mL)[OBS]
PERibru  : Perif compartment for ibrutinib   ()[obs, adm]
CENTdhd  : Central compartment for DHD       (ng/mL)[OBS]
PERdhd   : Perif compartment for DHD         ()[]

$GLOBAL
double PAR, MET, DV, K20, K23, K32, K24, K45, K54, K40, D, ALAG, CLibru, V2, V3, CLmet, KAdhd, CLdhd, K12, K14 ;

$TABLE
PAR = (CENTibru / V2) * (1 + EPS(1)) ;
MET = (CENTdhd  / V2) * (1 + EPS(3)) ;

if(self.cmt == 4) {
    DV = MET ;
} else {
    DV = PAR ;
}

$MAIN
V3 = TVV3 * exp (ETA(1) + ETA1) ;
V2 = TVV2 * exp (ETA(2) + ETA2) ;


KAdhd = TVKAdhd * exp (ETA(5) + ETA5) ;
CLmet = TVCLmet * exp (ETA(6) + ETA6) ; 
ALAG  = TVALAG  * exp (ETA(7) + ETA7) ;
D     = TVD     * exp (ETA(8) + ETA8) ;

if (CYCLE == 1){
    CLdhd  = TVCLdhd  * exp (ETA(3) + ETA3 + ETA(11) + ETA11) ;
    CLibru = TVCLibru * exp (ETA(4) + ETA4 + ETA(9)  + ETA9 ) ;
} else {
    CLdhd  = TVCLdhd  * exp (ETA(3) + ETA3 + ETA(12) + ETA12) ;
    CLibru = TVCLibru * exp (ETA(4) + ETA4 + ETA(10) + ETA10) ;
}


K20 = CLibru / V2 ;
K23 = Qibru  / V2 ;
K32 = Qibru  / V3 ;
K24 = CLmet  / V2 ;
K45 = Qdhd   / V2 ;
K54 = Qdhd   / V3 ;
K40 = CLdhd  / V2 ;
K12 = KAibru      ;
K14 = KAdhd       ;


D_EV = D ;
ALAG_EV = ALAG ;


$ODE
dxdt_EV       = - K12 * EV - K14 * EV ;
dxdt_CENTibru =   K12 * EV - K20 * CENTibru - K23 * CENTibru - K24 * CENTibru + K32 * PERibru ;
dxdt_PERibru  =   K23 * CENTibru - K32 * PERibru ;
dxdt_CENTdhd  =   K14 * EV + K24 * CENTibru - K45 * CENTdhd + K54 * PERdhd - K40 * CENTdhd ;
dxdt_PERdhd   =   K45 * CENTdhd - K54 * PERdhd ;

$CAPTURE @annotated
PAR: Plasma concentration Ibrutinib
MET: Plasma concentration DHDIbru
DV : Plasma concentration of cmt

ETA1 : eta V3
ETA2 : eta V2
ETA3 : eta CLdhd
ETA4 : eta CLibru
ETA5 : eta KAdhd
ETA6 : eta CLmet
ETA7 : eta ALAG
ETA8 : eta D
ETA9 : IOV_CLibru
ETA10: IOV_CLibru
ETA11: IOV_CLdhd 
ETA12: IOV_CLdhd 