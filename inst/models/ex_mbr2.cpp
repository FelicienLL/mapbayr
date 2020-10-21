$PROB

- drug: Examplinib2
- model_ref: XXX, J Pharmacokinet, 2020

$PARAM @annotated
TVKA    : 1   : Absorption constant (h-1)
TVVC    : 10  : Central volume (L)
TVCLPAR : 1 : Clearance parent drug (L/h)
TVCLMET : 0.8 : Clearance metabolite (L/h)
TVFM    : 0.7 : Fraction metabolized (L/h)

D   : 1.5 : Zero-order absorption duration (h)
LAG : .6  : Lag time (h)
Q   : 1   : Intercompartmental clearance (L/h)
VP  : 20  : Peripheral volume (L)

ETA1 : 0 : KA (h-1)
ETA2 : 0 : VC (L)
ETA3 : 0 : CLpar (L/h)
ETA4 : 0 : CLmet (L/h)
ETA5 : 0 : FM ()

$OMEGA 
0.2
0.4
0.2
0.1
0.6

$SIGMA
0.15 // prop parent
0    // add parent
0.10 // prop metabolite
0    // add metabolite

$CMT @annotated
DEPOT     : Depot compartment () [ADM]
CENTPAR   : Central compartment of parent drug (ng/mL) [OBS]
PERIPHPAR : Peripheral compartment of parent drug (ng/mL) []
CENTMET   : Central compartment of metabolite  (ng/mL)[OBS]
PERIPHMET : Peripheral compartment of metabolite (ng/mL) []

$TABLE
double PAR = (CENTPAR / VC) * (1 + EPS(1)) ;
double MET = (CENTMET / VC) * (1 + EPS(3)) ;
double DV = PAR ;
if(self.cmt == 4)  DV = MET ;

$MAIN
double KA    = TVKA    * exp(ETA1 + ETA(1)) ;
double VC    = TVVC    * exp(ETA2 + ETA(2)) ;
double CLPAR = TVCLPAR * exp(ETA3 + ETA(3)) ;
double CLMET = TVCLMET * exp(ETA4 + ETA(4)) ;
double FM    = exp(log(TVFM/(1-TVFM))+(ETA5 + ETA(5)))/(1+exp(log(TVFM/(1-TVFM))+(ETA5 + ETA(5)))) ;

double K12 = KA ;
double K24 = CLPAR/VC * FM ;
double K23 = Q / VC   ;
double K32 = Q / VP   ;
double K20 = CLPAR/VC * (1 - FM) ;
double K45 = Q / VC   ;
double K54 = Q / VP   ;
double K40 = CLMET/VC ;

D_DEPOT    = D   ;
ALAG_DEPOT = LAG ;

$ODE
dxdt_DEPOT     = - K12 * DEPOT ;
dxdt_CENTPAR   =   K12 * DEPOT   + K32 * PERIPHPAR - (K20 + K24 + K23) * CENTPAR ;
dxdt_PERIPHPAR =   K23 * CENTPAR - K32 * PERIPHPAR ;
dxdt_CENTMET   =   K24 * CENTPAR + K54 * PERIPHMET - (K40 + K45) * CENTMET ;
dxdt_PERIPHMET =   K45 * CENTMET - K54 * PERIPHMET  ;

$CAPTURE @annotated
PAR: Central concentration parent drug
MET: Central concentration metabolite
DV : Concentration to fit 

ETA1 : KA
ETA2 : VC
ETA3 : CLpar
ETA4 : CLmet
ETA5 : FM