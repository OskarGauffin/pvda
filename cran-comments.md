## Resubmission

This is a resubmission. One unix build (r-devel-linux-x86_64-fedora-gcc) failed at the Vignette-building after the package had been sent to CRAN.

* I think the issue was a suffix of a png-file which was written with capital letters (".PNG") which I've now corrected. Resubmitting and hoping that does the trick.  

-------------------------------------------------------
This is a resubmission. In this version I've adressed all comments the last reviewer,

* I've updated the package description to be more informative, omitting "the pvda package" from the beginning.
* Added the references I had in the function documentation to the DESCRIPTION. 
* Included the \Value in the three functions which missed these. 
* Beni thought the \dontrun on examples seemed unwarranted. I've removed it now, and run examples on a smaller dataset to reduce the runtime, hopefully now below 5 sec in the automated CRAN checks.  

------------------------------------------------------
This is a resubmission. In this version I addressed a comment from the last reviewer,

* I've removed the "+ file LICENSE" from DESCRIPTION as well as the LICENSE-file.

------------------------------------------------------
This is a resubmission. In this version I've adressed data-table using too many cores by:

* I've wrapped all examples with \dontrun. 
* Set the Sys.setenv("OMP_THREAD_LIMIT" = 2) in my testthat.R-file
* Set data.table::setDTthreads(1) in the beginning of the vignette.

------------------------------------------------------

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
