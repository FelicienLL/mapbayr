$PROB

- drug: Imatinib
- model_name: Imatinib Delbaldo
- model_ref: Delbaldo et al, Cancer Therapy Clinical, 2006
- error_model: Prop
- adm_cmt: 1
- obs_cmt: 1,2
- concentration_unit: ng/mL

$PARAM @annotated
TVCLimat  : 7.97  : Imatinib clearance (L/h)
CLimatAAG : -0.52 : Effect of AAG on imatinib clearance ()
TVVimat   : 168   : Imatinib Volume of distribution (L)
TVD1      : 2.53  : Infusion duration (h)
TVCLcgp   : 58.6  : CGP clearance (L/h)
CLcgpAAG  : -0.6  : Effect of AAG on CGP clearance ()
CLcgpDAY30  : 0.55  : Effect of OCC on CGP clearance ()
TVVcgp    : 15.4  : CGP volume of distribution (L)

ETA1 : 0 : CLimat (L/H)
ETA2 : 0 : Vimat (L)
ETA3 : 0 : D1 (h)
ETA4 : 0 : CLcgp (L/h)
ETA5 : 0 : Vcgp (h)
ETA6 : 0 : D1_IOV1 (h)
ETA7 : 0 : D1_IOV2 (h)

$PARAM @annotated @covariates
AAG : 1.15 : AAG (g/L)
DAY30 : 0 : PK data obtained on day <30 = 0, or >=30 =1 ()

$OMEGA
0.0841
0.3844
0.64
0.0529
0.5184
0.1444
0.1444

$SIGMA
0.050176
0
0.071289
0

$CMT @annotated
CENTimat : Central compartment for imatinib
CENTcgp : Central compartment for CGP

$GLOBAL
double PAR, MET, DV, CLimat, CLcgp, K12, K20, D1, Vimat, Vcgp, OCC, OCC1, OCC2 ;

$TABLE
PAR = (CENTimat / Vimat) * (1 + EPS(1)) ;
MET = (CENTcgp / Vcgp) * (1 + EPS(3)) ;

if(self.cmt == 2) {
    DV = MET ;
} else {
    DV = PAR ;
}


$MAIN
OCC1 = 1 - DAY30 ;
OCC2 = DAY30 ;

D1 = TVD1 * exp (ETA(3) + ETA3) * exp (OCC1 * (ETA(6) + ETA6) + OCC2 * (ETA(7) + ETA7) ) ;
D_CENTimat = D1 ;

Vimat = TVVimat * exp (ETA(2) + ETA2) ;
Vcgp = TVVcgp * exp (ETA(5) + ETA5) ;

CLimat = TVCLimat * pow ((AAG/1.15) , CLimatAAG) * exp (ETA(1) + ETA1) ;

if (DAY30 == 0) OCC = 0 ;
else OCC = 1 ;
CLcgp = TVCLcgp * pow ((AAG/1.15) , CLcgpAAG) * exp (ETA(4) + ETA4) * pow (CLcgpDAY30, DAY30);

K12 = CLimat/Vimat ;
K20 = CLcgp/Vcgp ;


$ODE
dxdt_CENTimat = - K12*CENTimat ;
dxdt_CENTcgp = K12*CENTimat - K20*CENTcgp ;

$CAPTURE @annotated
PAR :
MET :
DV : Plasma concentration (mass/volume) 
D1 :
Vimat : 
Vcgp : 
CLcgp :


  
  