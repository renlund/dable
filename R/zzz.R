## .onLoad <- function(libname, pkgname){
##     op <- options()
##     op.dable <- dable_defaults
##     toset <- !(names(op.dable) %in% names(op))
##     if(any(toset)) options(op.dable[toset])
##     invisible()
## }

.onLoad <- function(libname, pkgname){
    dp_apply_defaults(overwrite = FALSE)
    invisible()
}
