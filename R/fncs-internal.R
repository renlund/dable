##' internal helper functions
##'
##' functions that work properly inside 'dable'
##' @param iinfo character, the name of internal info accessible to functions
##' @param ... arguments passed from within 'dable'
##' @name dable-internal
NULL

##' @rdname dable-internal
##' @details di.dots: extract info from the '...'
##' @param strict logical, if TRUE return NULL when iinfo not found, else
##'     non-empty string
##' @export
di.dots <- function(iinfo, strict = FALSE, ...){
    properties(strict, class = "logical", length = 1, na.ok = FALSE)
    properties(iinfo, class = "character", length = 1, na.ok = FALSE)
    one_of(iinfo, set = c("term", "label", "type", "group",
                         "missing", "table.type"))
    dots <- list(...)
    r <- dots[[paste0(".", iinfo)]]
    if(is.null(r) & strict){
        s <- paste0("[di.dots] '", iinfo, "' not found in ... (the dots)")
        message(s)
        NULL
    } else if(is.null(r)){
        paste0(iinfo, " not found")
    } else r
}

## di.dots("label")
## di.dots("label", strict = TRUE)

## sep and indent should be package options
var_lev_combine <- function(v, l, sep = ": ", indent = "   "){
    n <- length(l)
    if(n == 1){
        paste0(v, sep, l)
    } else {
        paste0(rep(c(paste0(v, sep), ""), c(1, n-1)),
               paste0(rep(c("", indent), c(1, n-1)), l))
    }
}
## var_lev_combine("Foo", letters[1:3])

##' @rdname dable-internal
##' @details di.Variable: create a 'Variable' column
##' @param x the input variable
##' @param lab.only logical, if TRUE don't extend with factorial levels
##' @export
di.Variable <- function(x, ...){
    lab <- di.dots("label", strict = TRUE, ...)
    type <- di.dots("type", strict = TRUE, ...)
    ## browser()
    ## START: this part somewhat superfluous, but is nice for testing
    if(is.null(lab)){
        lab <- deparse(substitute(x))
    }
    if(is.null(type)){
        type <- "real"
        if("factor" %in% class(x)) type <- "catg"
        if("character" %in% class(x)){
            type <- "catg"
            x <- factor(x)
        }
    }
    ## : END
    lev <- levels(x)
    if(type %in% c("real", "lcat", "date", "surv")){
        lab
    } else if(type %in% "catg"){
        var_lev_combine(v = lab, l = lev)
    } else if(type %in% "bnry"){
        var_lev_combine(v = lab, l = lev[2])
    } else stop("[di.Variable] out of options")
}

## di.Variable <- function(x, lab.only = FALSE, ...){
##     properties(lab.only, class = "logical", length = 1, na.ok = FALSE)
##     lab <- di.dots("label", strict = TRUE, ...)
##     if(is.null(lab)){
##         lab <- deparse(substitute(x))
##     }
##     lev <- levels(x)
##     if(lab.only || is.null(lev)){
##         lab
##     } else {
##         var_lev_combine(v = lab, l = lev)
##     }
## }


## foo <- 1:7
## di.Variable(foo)
## di.Variable(1:7)
## foo <- factor(sample(letters[1:3]))
## di.Variable(foo)
