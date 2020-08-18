$PROB

- drug: Cabozantinib
- model_name: Cabozantinib Nguyen
- model_ref: Nguyen et al, J Clin Pharmacol, 2019
- error_model: expo
- adm_cmt: 1, 2
- obs_cmt: 2
- concentration_unit: mg/L

$PARAM @annotated
TVKA     : 1.24   : Absorption constant (h-1)
D2       : 2.48   : Duration of absorption (h)
TVCL     : 2.48   : Clearance (L.h-1)
TVVC     : 212    : Central volume (L)
Q        : 30.0   : Intercompartmental clearance (L.h-1)
VP       : 177    : Peripheral volume (L)
ALAG1    : 0.821  : Lag time (h)
TVFR     : 0.83   : Fraction of Depot 1 ()
KA_DOSE  : 0.734  : Effect of DOSE  on KA ()
CL_AGE   : -0.157 : Effect of AGE   on CL ()
CL_SEX   : 0.76   : Effect of SEX   on CL ()
CL_WT    : -0.0393: Effect of WT    on CL ()
CL_RCC   : 0.87   : Effect of RCC   on CL ()
CL_CRPC  : 0.989  : Effect of CRPC  on CL ()
CL_MTC   : 1.9    : Effect of MTC   on CL ()
CL_GB    : 1.2    : Effect of GB    on CL ()
CL_OTHER : 1.19   : Effect of OTHER on CL ()
CL_HCC   : 0.878  : Effect of HCC   on CL ()
VC_AGE   : 0.0644 : Effect of AGE   on VC ()
VC_SEX   : 1.1    : Effect of SEX   on VC ()
VC_WT    : 1.19   : Effect of WT    on VC ()
VC_RCC   : 0.656  : Effect of RCC   on VC ()
VC_CRPC  : 0.743  : Effect of CRPC  on VC ()
VC_MTC   : 0.936  : Effect of MTC   on VC ()
VC_GB    : 0.479  : Effect of GB    on VC ()
VC_OTHER : 0.762  : Effect of OTHER on VC ()
VC_HCC   : 0.847  : Effect of HCC   on VC ()

ETA1 : 0 : KA (h-1)
ETA2 : 0 : CL (L/h)
ETA3 : 0 : VC (L)
ETA4 : 0 : FR ()

$PARAM @annotated @covariates

AOLA  : 40 : Amount of last administration (mg)
AGE   : 60 : Age                           (years)
SEX   : 0  : Sex                           (0=male, 1=female)
WT    : 80 : Body weight                   (kg)
RCC   : 0  : Renal cell cancer             (0=no, 1=yes)
CRPC  : 0  : Castr resist prostate cancer  (0=no, 1=yes)
MTC   : 0  : Metast med thyroid cancer     (0=no, 1=yes)
GB    : 0  : Glioblastoma                  (0=no, 1=yes)
OTHER : 0  : Other malignancies            (0=no, 1=yes)
HCC   : 0  : Hepatocellular carcinoma      (0=no, 1=yes)

$OMEGA @block
2.02
0.00 0.213
0.00 0.211 0.443
0.00 0.000 0.000 2.55

$SIGMA
0.000 // prop
0.118 // (log) additive

$CMT @annotated
DEPOT   : Depot      (mg)
CENTRAL : Central    (mg)
PERIPH  : Peripheral (mg)

$GLOBAL
double K12, K23, K32, K20, KA, CL, VC, PHI, FR, TVCL_COV, TVVC_COV ;

$TABLE
double DV  = (CENTRAL / VC) * exp(EPS(2)) ;

$MAIN
KA = TVKA * pow(AOLA/60, KA_DOSE) * exp(ETA1 + ETA(1)) ;

TVCL_COV = TVCL * pow(AGE/60, CL_AGE) * pow(CL_SEX, SEX) * pow(WT/80, CL_WT)  * pow(CL_RCC, RCC) * pow(CL_CRPC, CRPC) * pow(CL_MTC, MTC) * pow(CL_GB, GB) * pow(CL_OTHER, OTHER) * pow(CL_HCC, HCC) ;
CL = TVCL_COV * exp(ETA2 + ETA(2))  ;

TVVC_COV = TVVC * pow(AGE/60, VC_AGE) * pow(VC_SEX, SEX) * pow(WT/80, VC_WT)  * pow(VC_RCC, RCC) * pow(VC_CRPC, CRPC) * pow(VC_MTC, MTC) * pow(VC_GB, GB) * pow(VC_OTHER, OTHER) * pow(VC_HCC, HCC) ;
VC = TVVC_COV * exp(ETA3 + ETA(3))  ;

FR= exp(log(TVFR/(1-TVFR))+(ETA4 + ETA(4)))/(1+exp(log(TVFR/(1-TVFR))+(ETA4 + ETA(4)))) ;

K12 = KA      ;
K23 = Q  / VC ;
K32 = Q  / VP ;
K20 = CL / VC ;

F_DEPOT    = FR  ;
ALAG_DEPOT = ALAG1 ;

F_CENTRAL  = 1 - FR ;
D_CENTRAL  = D2 ;
ALAG_CENTRAL = 0.000001 ;

$ODE
dxdt_DEPOT   = - K12 * DEPOT                                          ;
dxdt_CENTRAL =   K12 * DEPOT   + K32 * PERIPH - (K23 + K20) * CENTRAL ;
dxdt_PERIPH  =   K23 * CENTRAL - K32 * PERIPH                         ;

$CAPTURE @annotated
DV : mg/L
ETA1 : KA (h-1)
ETA2 : CL (L/h)
ETA3 : VC (L)
ETA4 : FR ()
