# Read compartment options in a model

Read compartment options in a model

## Usage

``` r
adm_cmt(x)

obs_cmt(x)
```

## Arguments

- x:

  model object

## Value

a vector of compartment identified as default "administration" or
"observation" compartments.

## Details

In a mrgsolve model, it is possible to specify options in `$CMT`. If
`[ADM]` or `[OBS]` are set, mapbayr will interpret these as defaults
administration and observation compartments, respectively.

## Examples

``` r
#Administration:  Both 1st and 0- order
model <- exmodel(6, compile = FALSE)
mrgsolve::see(model)
#> 
#> Model file:  mrg_006.cpp 
#> $PROB LAG model 
#> 
#> $PARAM @annotated
#> TVCL   : 4.00 : Clearance (L/h)
#> TVVC   : 70.0 : Central volume of distribution (L)
#> TVKA   : 1.00 : Absorption rate (h-1)
#> TVD2   : 4.00 : Zero-order constant (h)
#> FR     : 0.2  : Fraction absorbed from Depot 1 ()
#> 
#> ETA1 : 0 : CL
#> ETA2 : 0 : VC
#> ETA3 : 0 : KA
#> ETA4 : 0 : D2
#> 
#> $OMEGA
#> 0.2 // CL
#> 0.2 // VC
#> 0.2 // KA 
#> 0.2 // D2
#> 
#> $SIGMA 
#> 0.05 // err prop
#> 0   //  err additive 
#> 
#> 
#> $CMT @annotated
#> DEPOT1   : Depot () [ADM]
#> CENTRAL : Central () [ADM, OBS]
#> 
#> $TABLE
#> double DV = (CENTRAL / VC) * (1 + EPS(1)) + EPS(2) ;
#> 
#> $MAIN
#> double CL  = TVCL  * exp(ETA(1) + ETA1 ) ; 
#> double VC  = TVVC  * exp(ETA(2) + ETA2 ) ;
#> double KA  = TVKA  * exp(ETA(3) + ETA3 ) ;
#> double D2  = TVD2  * exp(ETA(4) + ETA4 ) ;
#> double K20 = CL / VC ;
#> 
#> F_DEPOT1  = FR ;
#> F_CENTRAL = 1 - FR ; 
#> D_CENTRAL = D2 ;
#> 
#> $ODE
#> dxdt_DEPOT1   = - KA * DEPOT1 ;
#> dxdt_CENTRAL  = - K20 * CENTRAL + KA * DEPOT1 ;
#> 
#> $CAPTURE DV
adm_cmt(model)
#> [1] 1 2

#Observation: Both parent drug and metabolite
model <- exmodel(401, compile = FALSE)
mrgsolve::see(model)
#> 
#> Model file:  mrg_401.cpp 
#> $PROB Reference model 
#> 
#> $PARAM @annotated
#> TVCL   : 4.00 : Clearance (L/h)
#> TVVC   : 70.0 : Central volume of distribution (L)
#> TVKA   : 1.00 : Absorption rate (h-1)
#> TVCLmet : 2.5 : Clerance metabolite (L/h)
#> TVVCmet : 60 : Central volume metabolite (L)
#> 
#> ETA1 : 0 : CL
#> ETA2 : 0 : VC
#> ETA3 : 0 : KA
#> ETA4 : 0 : CLmet
#> ETA5 : 0 : VCmet
#> 
#> $OMEGA
#> 0.2 // CL
#> 0.2 // VC
#> 0.2 // KA 
#> 0.2 // CLmet
#> 0.2 // VCmet
#> 
#> $SIGMA 
#> 0.05 // err prop parent
#> 0   //  err additive parent
#> 0.05 // err prop metabolite
#> 0   //  err additive metabolite
#> 
#> $CMT @annotated
#> DEPOT   : Depot () [ADM]
#> CENTRAL : Central () [OBS]
#> CENTRALMET : Central metabolite () [OBS]
#> 
#> $TABLE
#> double PAR = (CENTRAL / VC) * (1 + EPS(1)) + EPS(2) ;
#> double MET = (CENTRALMET / VCmet) * (1 + EPS(3)) + EPS(4) ;
#> double DV = PAR ;
#> if(self.cmt == 3) DV = MET ;
#> 
#> $MAIN
#> double CL     = TVCL     * exp(ETA(1) + ETA1 ) ; 
#> double VC     = TVVC     * exp(ETA(2) + ETA2 ) ;
#> double KA     = TVKA     * exp(ETA(3) + ETA3 ) ;
#> double CLmet  = TVCLmet  * exp(ETA(4) + ETA4 ) ; 
#> double VCmet  = TVVCmet  * exp(ETA(5) + ETA5 ) ;
#> 
#> double K23 = CL / VC ;
#> double K30 = CLmet / VCmet ; 
#> 
#> $ODE
#> dxdt_DEPOT      = - KA * DEPOT ;
#> dxdt_CENTRAL    = - K23 * CENTRAL + KA * DEPOT ;
#> dxdt_CENTRALMET = 0.7 * K23 * CENTRAL - K30 * CENTRALMET ;
#> 
#> $CAPTURE DV PAR MET
obs_cmt(model)
#> [1] 2 3
```
