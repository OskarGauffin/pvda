# .onLoad <- function(libname, pkgname) {
#   base::packageStartupMessage("Under development. Report issues at https://github.com/OskarGauffin/pvda")
# }

# .onLoad <- function(libname, pkgname) {
#   # CRAN OMP THREAD LIMIT
#   # This did not, on it's own, prevent NOTEs about too many cores being used by data.table, when
#   # submitting to CRAN.
#   Sys.setenv("OMP_THREAD_LIMIT" = 2)
# }
