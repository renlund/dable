dable_parameters <- list(
    dable.bnry.list = list(c(0, 1),
                            c(FALSE, TRUE),
                            c("No", "Yes"),
                            c("no", "yes")),
    dable.real.tol = 0,
    dable.catg.tol = Inf,
    dable.surv.prefix = TRUE,
    dable.surv.affix = c("time" = "t.", "event" = "ev."),
    dable.unit.id = NULL,
    dable.gtab.defvar.rm = TRUE,
    dable.weight.defvar.rm = TRUE,
    dable.vtab.group.name = "Covariates",
    dable.stab.group.name = "Time-to-event",
    dable.gtab.group.name = "All",
    ## printingish
    dable.units.name = "subjects",
    dable.grey.first = FALSE,
    ## formatting
    dable.digits = c("small" = 2, "mid" = 2, "large" = 1),
    dable.sc = TRUE,
    dable.sc.low = 1e-8,
    dable.sc.high = 1e8,
    dable.p = FALSE,
    dable.p.bound = 1e-4,
    dable.NAtext = "",
    dable.NAalias = "Missing",
    dable.output = "latex",
    dable.sep = ": " ,
    dable.indent = "    ",
    ## default describers --------------------
    dable.real.desc = "mean_sd", ## "tester2"
    dable.bnry.desc = "bnry.count_prop",
    dable.catg.desc = "catg.count_prop",
    dable.lcat.desc = "n.unique",
    dable.date.desc = "min_max",
    dable.surv.desc = "eventrate",
    ## default comparers
    dable.real.comp = "real.std",
    dable.bnry.comp = "bnry.std",
    dable.catg.comp = "catg.std",
    dable.lcat.comp = "bnry.std",
    dable.date.comp = "date.std",
    dable.surv.comp = "rate_ratio",
    ## default testers
    dable.real.test = "param",
    dable.bnry.test = "bnry.chisq",
    dable.catg.test = "catg.chisq",
    dable.lcat.test = "bnry.chisq",
    dable.date.test = "date.nonparam",
    dable.surv.test = "logrank",
    ## default baseline describers -----------
    dable.real.desc.bl = "mean_sd.bl",
    dable.bnry.desc.bl = "bnry.count_prop.bl",
    dable.catg.desc.bl = "catg.count_prop.bl",
    dable.lcat.desc.bl = "lcat.bl",
    dable.date.desc.bl = "date.bl",
    dable.surv.desc.bl = "eventrate.bl",
    ## default baseline comparers
    dable.real.comp.bl = "real.std",
    dable.bnry.comp.bl = "bnry.std",
    dable.catg.comp.bl = "catg.std",
    dable.lcat.comp.bl = "bnry.std",
    dable.date.comp.bl = "date.std",
    dable.surv.comp.bl = "surv.std",
    ## default baseline tester
    dable.real.test.bl = "param.bl",
    dable.bnry.test.bl = "bnry.chisq.bl",
    dable.catg.test.bl = "catg.chisq.bl",
    dable.lcat.test.bl = "bnry.chisq.bl",
    dable.date.test.bl = "date.nonparam.bl",
    dable.surv.test.bl = "logrank.bl"
)

dp_apply_defaults <- function(overwrite = FALSE){
    if(overwrite){
        options(dable_parameters)
    } else {
        op <- options()
        toset <- !(names(dable_parameters) %in% names(op))
        if(any(toset)) options(dable_parameters[toset])
    }
    invisible()
}

## make sure parameter name follows standard for options (i.e. begins with 'dable.')
optName <- function(x){
   ifelse(!grepl("^dable\\.", x), yes = paste0("dable.", x), no = x)
}

## make sure parameter name follows standard for abbreviation (i.e. does not
## begin with 'dable.')
pName <- function(x){
    ifelse(grepl("^dable\\.", x), yes = sub("^dable\\.", "", x), no = x)
}

##' @title set package parameter
##' @description set a dable package parameter
##' @param param character; name of parameter
##' @param value the value to be set
##' @export
dpset <- function(param, value = NULL){
    properties(param, class = "character", length = 1, na.ok = FALSE)
    one_of(pName(param), nm = "name of parameter",
           set = pName(names(dable_parameters)))
    value <- dparam(param = param, value = value)
    op <- list()
    op[[optName(param)]] <- value
    options(op)
    invisible()
}

##' @describeIn dpset dpget: get package parameter
##' @param verbose logical; get(possibly) helpful messages?
##' @export
dpget <- function(param, verbose = FALSE){
    r <- options(optName(param))[[1]]
    if(verbose & is.null(r)){
        message(paste0("no default for '", pName(param), "' found"))
    }
    r
}
dable.default <- dpget

##' @describeIn dpset dp_restore_defaults: restores the dable package parameter defaults
##' @export
dp_restore_defaults <- function() dp_apply_defaults(overwrite = TRUE)

##' @describeIn dpset add2bnry.list: add a value to the 'bnry.list'
##' @export
add2bnry.list <- function(value){
    if(!is.list(value)) value <- list(value)
    dpset(param = "bnry.list", value = c(dpget("bnry.list"), value))
}

## get parameter value and (in some cases) perform a sanity test
dparam <- function(param, value = NULL){
    properties(param, class = "character", length = 1, na.ok = FALSE)
    p <- pName(param)
    one_of(p, nm = "name of parameter", set = pName(names(dable_parameters)))
    switch(
        EXPR = p,
        bnry.list = dp_bnry.list(value),
        real.tol = dp_real.tol(value),
        catg.tol = dp_catg.tol(value),
        ## surv.prefix = dp_surv.prefix(value),
        surv.prefix = logic1(value, p),
        surv.affix = dp_surv.affix(value),
        ## unit.id = dp_unit.id(value),
        unit.id = char1(value, p),
        ## gtab.defvar.rm = dp_gtab.defvar.rm(value),
        gtab.defvar.rm = logic1(value, p),
        ## weight.defvar.rm = dp_weight.defvar.rm(value),
        weight.defvar.rm = logic1(value, p),
        ## stab.group.name = dp_stab.group.name(value),
        stab.group.name = char1(value, p),
        ## vtab.group.name = dp_vtab.group.name(value),
        vtab.group.name = char1(value, p),
        ## gtab.group.name = dp_gtab.group.name(value),
        gtab.group.name = char1(value, p),
        ## units.name = dp_units.name(value),
        units.name = char1(value, p),
        digits = dp_digits(value),
        ## sc = dp_sc(value),
        sc = logic1(value, p),
        ## sc.low = dp_sc.low(value),
        sc.low = positive1(value, p),
        ## sc.high = dp_sc.high(value),
        sc.high = positive1(value, p),
        ## p = dp_p(value),
        p = logic1(value, p),
        ## p.bound = dp_p.bound(value),
        p.bound = positive1(value, p),
        ## NAtext = dp_NAtext(value),
        NAtext = char1(value, p),
        NAalias = char1(value, p),
        ## grey.first = dp_grey.first(value),
        grey.first = logic1(value, p),
        output = char1(value, p),
        dable.sep = char1(value, p),
        dable.indent = char1(value, p),
        if(is.null(value)) dpget(p) else value
    )
}

## dp_-functions below are sanity tests for parameter values
dp_bnry.list <- function(x = NULL){
    if(is.null(x)) x <- dpget("bnry.list")
    properties(x, class = "list")
    foo <- function(z){
        length(z) == 2 &&
            all(!is.na(z)) &&
            z[1] != z[2]
    }
    l <- unlist(lapply(x, foo))
    if(!is.null(l) && any(!l)){
        s <- paste0("the bnry.list needs to be a list of length 2 vectors ",
                    "with unique values not containing any missing")
        stop(s)
    }
    x
}

dp_real.tol <- function(x = NULL){
    if(is.null(x)) x <- dpget("real.tol")
    properties(x, class = c("numeric", "integer"),
               length = 1, na.ok = FALSE)
    if(x < Inf && x != as.integer(x)){
        s <- paste0("real.tol should be integer (or Inf)")
        x <- as.integer(x)
        warning(s)
    }
    if(x < 0){
        s <- paste0("real.tol should be a non-negative integer")
        warning(s)
    }
    x
}

dp_catg.tol <- function(x = NULL){
    if(is.null(x)) x <- dpget("catg.tol")
    properties(x, class = c("numeric", "integer"),
               length = 1, na.ok = FALSE)
    if(x < Inf && x != as.integer(x)){
        s <- paste0("catg.tol should be integer (or Inf)")
        x <- as.integer(x)
        warning(s)
    }
    if(x < 1){
        s <- paste0("catg.tol should be a positive integer")
        warning(s)
    }
    x
}

## a test for this param also exists in import-fncs-stab.R
dp_surv.affix <- function(x = NULL){
    if(is.null(x)) x = dpget("surv.affix")
    properties(x, class = "character", length = 2, na.ok = FALSE)
    properties(names(x), nm = "names of affix", class = "character",
               length = 2, na.ok = FALSE)
    inclusion(names(x), nm = "names of affix", include = c("time", "event"))
    if(names(x)[1] != "time"){
        stop("first component of 'affix' should be named 'time'")
    }
    x
}

dp_digits <- function(x = NULL){
    if(is.null(x)) x <- dpget("digits")
    properties(x, nm = "digits", class = c("numeric", "integer"),
               length = 3, na.ok = FALSE)
    if(!setequal(names(x), c("small", "mid", "large"))){
        s <- paste0("'digits' should be a named length 3 vector ",
                    "(names: small, mid, large)")
        stop(s)
    }
    x
}

char1 <- function(x, nm){
    if(is.null(x)) x <- dpget(nm)
    properties(x, nm = nm, class = "character", length = 1, na.ok = FALSE)
    x
}

logic1 <- function(x, nm){
    if(is.null(x)) x <- dpget(nm)
    properties(x, nm = nm, class = "logical", length = 1, na.ok = FALSE)
    x
}

positive1 <- function(x, nm){
    if(is.null(x)) x <- dpget(nm)
    properties(x, nm = nm, class = "numeric", length = 1, na.ok = FALSE)
    if(x < 0){
        s <- paste0("'", nm,"' should be positive")
        stop(s)
    }
    x
}

## a test for this param also exists in import-fncs-stab.R
## dp_surv.prefix <- function(x = NULL){
##     if(is.null(x)) x <- dpget("surv.prefix")
##     properties(x, nm = "surv.prefix", class = "logical",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_unit.id <- function(x = NULL){
##     if(is.null(x)) x <- dpget("unit.id")
##     if(!is.null(x)){
##         properties(x, nm = "unit.id", class = "character",
##                    length = 1, na.ok = FALSE)
##     }
##     x
## }

## dp_gtab.defvar.rm <- function(x = NULL){
##     if(is.null(x)) x <- dpget("gtab.defvar.rm")
##     properties(x, nm = "gtab.defvar.rm", class = "logical",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_weight.defvar.rm <- function(x = NULL){
##     if(is.null(x)) x <- dpget("weight.defvar.rm")
##     properties(x, nm = "weight.defvar.rm", class = "logical",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_vtab.group.name <- function(x = NULL){
##     if(is.null(x)) x <- dpget("vtab.group.name")
##     properties(x, nm = "vtab.group.name", class = "character",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_stab.group.name <- function(x = NULL){
##     if(is.null(x)) x <- dpget("stab.group.name")
##     properties(x, nm = "stab.group.name", class = "character",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_gtab.group.name <- function(x = NULL){
##     if(is.null(x)) x <- dpget("gtab.group.name")
##     properties(x, nm = "gtab.group.name", class = "character",
##                length = 1, na.ok = FALSE)
##     x
## }


## dp_sc <- function(x = NULL){
##     if(is.null(x)) x <- dpget("sc")
##     properties(x, nm = "sc", class = "logical",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_sc.low <- function(x = NULL){
##     if(is.null(x)) x <- dpget("sc.low")
##     properties(x, nm = "sc.low", class = c("numeric"),
##                length = 1, na.ok = FALSE)
##     if(x < 0) stop("'sc.low' should be positive")
##     x
## }

## dp_sc.high <- function(x = NULL){
##     if(is.null(x)) x <- dpget("sc.high")
##     properties(x, nm = "sc.high", class = c("numeric"),
##                length = 1, na.ok = FALSE)
##     if(x < 0) stop("'sc.high' should be positive")
##     x
## }

## dp_p <- function(x = NULL){
##     if(is.null(x)) x <- dpget("p")
##     properties(x, nm = "p", class = "logical",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_p.bound <- function(x = NULL){
##     if(is.null(x)) x <- dpget("p.bound")
##     properties(x, nm = "p.bound", class = c("numeric"),
##                length = 1, na.ok = FALSE)
##     if(x < 0) stop("'p.bound' should be positive")
##     x
## }

## dp_NAtext <- function(x = NULL){
##     if(is.null(x)) x <- dpget("NAtext")
##     properties(x, nm = "NAtext", class = "character",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_units.name <- function(x = NULL){
##     if(is.null(x)) x <- dpget("units.name")
##     properties(x, nm = "units.name", class = "character",
##                length = 1, na.ok = FALSE)
##     x
## }

## dp_grey.first <- function(x = NULL){
##     if(is.null(x)) x <- dpget("grey.first")
##     properties(x, nm = "grey.first", class = "logical",
##                length = 1, na.ok = FALSE)
##     x
## }
