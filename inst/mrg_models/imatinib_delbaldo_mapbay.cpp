$PROB

- drug: Imatinib
- model_name: Imatinib Delbaldo
- model_ref: Delbaldo et al, Clinical Cancer Res, 2006
- error_model: Prop
- adm_cmt: 1
- obs_cmt: 1,2
- concentration_unit: ng/mL

$PARAM @annotated
TVCLIMAT  : 7.97  : Imatinib clearance (L/h)
COV_AAG_CLIMAT : -0.52 : Effect of AAG on imatinib clearance ()
TVVIMAT   : 168   : Imatinib Volume of distribution (L)
TVD1      : 2.53  : Infusion duration (h)
TVCLNDIM   : 58.6  : CGP clearance (L/h)
COV_AAG_CLNDIM  : -0.6  : Effect of AAG on CGP clearance ()
COV_DAY30_CLNDIM  : 0.55  : Effect of OCC on CGP clearance ()
TVVNDIM    : 15.4  : CGP volume of distribution (L)

ETA1 : 0 : CLIMAT (L/H)
ETA2 : 0 : VIMAT (L)
ETA3 : 0 : D1 (h)
ETA4 : 0 : CLCGP (L/h)
ETA5 : 0 : VCGP (h)
ETA6 : 0 : D1_IOV1 (h)
ETA7 : 0 : D1_IOV2 (h)

$PARAM @annotated @covariates
AAG : 1.15 : Acid alpha glycoprotein (g/L)
DAY30 : 0 : PK data day > 30 (<30 = 0, >=30 = 1)

$OMEGA
0.0841 // CLIMAT (L/H)
0.3844 // VIMAT (L)
0.64   // D1 (h)
0.0529 // CLCGP (L/h)
0.5184 // VCGP (h)
0.1444 // D1_IOV1 (h)
0.1444 // D1_IOV2 (h)

$SIGMA
0.050176
0
0.071289
0

$CMT @annotated
CENTIMAT : Central compartment for imatinib
CENTNDIM : Central compartment for CGP

$GLOBAL
double PAR, MET, DV, CLIMAT, CLNDIM, K12, K20, D1, VIMAT, VNDIM, OCC1, OCC2 ;

$TABLE
PAR = (CENTIMAT / VIMAT) * (1 + EPS(1)) ;
MET = (CENTNDIM / VNDIM) * (1 + EPS(3)) ;

if(self.cmt == 2) {
    DV = MET ;
} else {
    DV = PAR ;
}


$MAIN
OCC1 = 1 - DAY30 ;
OCC2 = DAY30 ;

D1 = TVD1 * exp (ETA(3) + ETA3) * exp (OCC1 * (ETA(6) + ETA6) + OCC2 * (ETA(7) + ETA7) ) ;
D_CENTIMAT = D1 ;

VIMAT = TVVIMAT * exp (ETA(2) + ETA2) ;
VNDIM = TVVNDIM * exp (ETA(5) + ETA5) ;

CLIMAT = TVCLIMAT * pow ((AAG/1.15) , COV_AAG_CLIMAT) * exp (ETA(1) + ETA1) ;
CLNDIM = TVCLNDIM * pow ((AAG/1.15) , COV_AAG_CLNDIM) * exp (ETA(4) + ETA4) * pow (COV_DAY30_CLNDIM, DAY30);

K12 = CLIMAT/VIMAT ;
K20 = CLNDIM/VNDIM ;


$ODE
dxdt_CENTIMAT = - K12*CENTIMAT ;
dxdt_CENTNDIM = K12*CENTIMAT - K20*CENTNDIM ;

$CAPTURE @annotated
PAR    :
MET    :
DV     :
D1     : 
CLIMAT : 
VIMAT  : 
CLNDIM : 
VNDIM  : 
ETA1   : CLIMAT (L/H)
ETA2   : VIMAT (L)
ETA3   : D1 (h)
ETA4   : CLCGP (L/h)
ETA5   : VCGP (h)
ETA6   : D1_IOV1 (h)
ETA7   : D1_IOV2 (h)