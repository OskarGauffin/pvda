## Resubmission

This is a resubmission. In this version I have: 

* Corrected four URLs in the README.md file. An old github repo name caused an "possibly invalid URL" as those URLs were redirected automatically to the new github repo name.

* Wrapped time consuming examples in the noted function in \dontrun. 

* For these two notes: 

"Running R code in ‘testthat.R’ had CPU time 10.2 times elapsed time" 
"Re-building vignettes had CPU time 7.3 times elapsed time"

I've followed advice from stack overflow and included an onLoad-function which limits the number of cores used (OMP_THREAD_LIMIT" = 2). 

For completeness, this is the post I followed https://stackoverflow.com/questions/77323811/r-package-to-cran-had-cpu-time-5-times-elapsed-time 
and this post suggests that it should cover the vignette note as well:
https://www.mail-archive.com/r-package-devel@r-project.org/msg08734.html

## Test environments
* local windows 10 machine, R 4.3.1
* github actions: ubuntu-latest (devel, release, oldrel-1), 
                  macos-latest (release), 
                  windows-latest (release)
                  
* rhub - Ubuntu Linux 20.04.1 LTS, R-release, GCC
         Windows Server 2022, R-devel, 64 bit
         Fedora Linux, R-devel, clang, gfortran

## Check results 
NOTES (all platforms together):

0 Errors | 0 Warnings

Possibly misspelled words in DESCRIPTION:
  disproportionality (7:48)
  Disproportionality (3:8)
  Pharmacovigilance (3:41)
  
COMMENT: These words are not misspelled. 
  
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
COMMENT: From questions and answers on stack overflow, these notes do not seem related to the submitted package. 

## Downstream dependencies
There are currently no downstream dependencies for this package. 
