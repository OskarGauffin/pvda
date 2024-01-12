# .onLoad <- function(libname, pkgname) {
#   base::packageStartupMessage("Under development. Report issues at https://github.com/OskarGauffin/pvda")
# }

.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 2)

}
