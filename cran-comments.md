## Resubmission
This is a resubmission. In this version I have:

* Changed the contact address in DESCRIPTION.
  
## Test environment
local Windows 10, R 4.2.0, 64 bit

R-hub (Windows Server 2022, R-devel, 64 bit ; Ubuntu Linux 20.04.1 LTS, R-release, GCC ; Fedora Linux, R-devel, clang, gfortran)

Mac OS M1 (macOS builder)

## R CMD check results
This is a patch of a package archived of CRAN due to test-fails during additional checks on Mac M1

Locally, there were no ERRORs, no WARNINGs, no NOTEs.

On r-hub, there was one NOTE:  

Possibly misspelled words in DESCRIPTION:
  mrgsolve (12:130)
  pharmacokinetic (12:78)
  posteriori (12:33)

However words are in the WORDLIST.
