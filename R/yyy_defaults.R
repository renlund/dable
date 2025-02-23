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
    dable.real.desc = "mean_sd",
    dable.catg.desc = "catg.count_prop",
    dable.bnry.desc = "bnry.count_prop",
    dable.lcat.desc = "n.unique",
    dable.date.desc = "min_max",
    dable.surv.desc = "eventrate",
    ## default comparers
    dable.real.comp = "real.std",
    dable.catg.comp = "catg.std",
    dable.bnry.comp = NULL,
    dable.lcat.comp = NULL,
    dable.date.comp = "date.std",
    dable.surv.comp = "surv.std",
    ## default testers
    dable.real.test = "param",
    dable.catg.test = "catg.chisq",
    dable.bnry.test = NULL,
    dable.lcat.test = NULL,
    dable.date.test = "date.nonparam",
    dable.surv.test = "logrank",
    ## default baseline describers -----------
    dable.real.desc.bl = "real.bl0",
    dable.catg.desc.bl = "catg.bl0",
    dable.bnry.desc.bl = "bnry.bl0",
    dable.lcat.desc.bl = "lcat.bl0",
    dable.date.desc.bl = "date.bl0",
    dable.surv.desc.bl = "surv.bl0",
    ## default baseline comparers
    dable.real.comp.bl = "real.std",
    dable.catg.comp.bl = "catg.std",
    dable.bnry.comp.bl = NULL,
    dable.lcat.comp.bl = NULL,
    dable.date.comp.bl = "date.std",
    dable.surv.comp.bl = "surv.std",
    ## default baseline tester
    dable.real.test.bl = "param.bl",
    dable.catg.test.bl = "catg.chisq.bl",
    dable.bnry.test.bl = NULL,
    dable.lcat.test.bl = NULL,
    dable.date.test.bl = "date.nonparam.bl",
    dable.surv.test.bl = "logrank.bl"
)

## make sure parameter name follows standard for options (i.e. begins with 'dable.')
optName <- function(x){
   ifelse(!grepl("^dable\\.", x), yes = paste0("dable.", x), no = x)
}

## make sure parameter name follows standard for abbreviation (i.e. does not
## begin with 'dable.')
pName <- function(x){
    ifelse(grepl("^dable\\.", x), yes = sub("^dable\\.", "", x), no = x)
}

##' dable parameter functions
##'
##' functions to set and retrieve dable parameters
##' @name parameter-fncs
NULL

##' @rdname parameter-fncs
##' @details set a dable package parameter
##' @param param character; name of parameter OR a named list of parameter values
##' @param value the value to be set
##' @export
dpset <- function(param, value = NULL){
    if(is.list(param)){
        for(i in seq_along(param)) dpset(param = names(param)[i],
                                         value = param[[i]])
    } else {
        properties(param, class = "character", length = 1, na.ok = FALSE)
        one_of(pName(param), nm = "name of parameter",
               set = pName(names(dable_parameters)))
        value <- dparam(param = param, value = value)
        op <- list()
        op[[optName(param)]] <- value
        options(op)
    }
    invisible()
}

##' @rdname parameter-fncs
##' @details set default parameter values
##' @param overwrite logical; overwrite parameter values already set?
##' @export
dpset_defaults <- function(overwrite = TRUE){
    if(overwrite){
        options(dable_parameters)
    } else {
        op <- options()
        toset <- !(names(dable_parameters) %in% names(op))
        if(any(toset)) options(dable_parameters[toset])
    }
    invisible()
}

subtypes.key <- setNames(object = c("catg", "catg"),
                         nm = c("bnry", "lcat"))

##' @rdname parameter-fncs
##' @details get package parameter
##' @param verbose logical; get (possibly) helpful messages?
##' @param sub logical; search for possible replacement values?
##' @export
dpget <- function(param, verbose = FALSE, sub = TRUE){
    r <- options(optName(param))[[1]]
    if(is.null(r) & sub){
        param2 <- NULL
        for(i in seq_along(subtypes.key)){
            sub <- names(subtypes.key)[i]
            main <- subtypes.key[i]
            if(grepl(pattern = sub, x = param)){
                param2 <- sub(pattern = sub, replacement = main,
                              x = param, fixed = TRUE)
                break
            }
        }
        if(!is.null(param2)) r <- options(optName(param2))[[1]]
    }
    if(verbose & is.null(r)){
        message(paste0("no default for '", pName(param), "' found"))
    }
    r
}
## dable.default <- dpget ##

##' @rdname parameter-fncs
##' @details get all package parameters
##' @export
dpget_all <- function(){
    z <- names(options())
    options()[grep(pattern = "^dable\\.", x = z, value = TRUE)]
}

##' @rdname parameter-fncs
##' @details add a value to the 'bnry.list'
##' @param bl list of binary values
##' @export
add2bnry.list <- function(bl){
    add <- dp_bnry.list(bl)
    dpset(param = "bnry.list", value = c(dpget("bnry.list"), add))
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
        surv.prefix = dp_logic1_(value, p),
        surv.affix = dp_surv.affix(value),
        ## unit.id = dp_unit.id(value),
        unit.id = dp_char1_(value, p, null.ok = TRUE),
        ## gtab.defvar.rm = dp_gtab.defvar.rm(value),
        gtab.defvar.rm = dp_logic1_(value, p),
        ## weight.defvar.rm = dp_weight.defvar.rm(value),
        weight.defvar.rm = dp_logic1_(value, p),
        ## stab.group.name = dp_stab.group.name(value),
        stab.group.name = dp_char1_(value, p),
        ## vtab.group.name = dp_vtab.group.name(value),
        vtab.group.name = dp_char1_(value, p),
        ## gtab.group.name = dp_gtab.group.name(value),
        gtab.group.name = dp_char1_(value, p),
        ## units.name = dp_units.name(value),
        units.name = dp_char1_(value, p),
        digits = dp_digits(value),
        ## sc = dp_sc(value),
        sc = dp_logic1_(value, p),
        ## sc.low = dp_sc.low(value),
        sc.low = dp_positive1_(value, p),
        ## sc.high = dp_sc.high(value),
        sc.high = dp_positive1_(value, p),
        ## p = dp_p(value),
        p = dp_logic1_(value, p),
        ## p.bound = dp_p.bound(value),
        p.bound = dp_positive1_(value, p),
        ## NAtext = dp_NAtext(value),
        NAtext = dp_char1_(value, p),
        NAalias = dp_char1_(value, p),
        ## grey.first = dp_grey.first(value),
        grey.first = dp_logic1_(value, p),
        output = dp_char1_(value, p),
        dable.sep = dp_char1_(value, p),
        ## dable.indent = dp_char1_(value, p),
        dable.indent = dp_indent(value, p),
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
        s <- paste0("the bnry.list needs to be a an empty list or a ",
                    "list of length 2 vectors with unique values ",
                    "not containing any missing")
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

dp_indent <- function(x){
    dp_char1(x = x, nm = "indent")
    if(x == ""){
        s <- paste0("indent should not be a zero length string")
        stop(s)
    }
    x
}

dp_char1_ <- function(x, nm, null.ok = FALSE){
    if(is.null(x)) x <- dpget(nm)
    if(null.ok && is.null(x)){
        "nothing needs doing"
    } else {
        properties(x, nm = nm, class = "character", length = 1, na.ok = FALSE)
    }
    x
}

dp_logic1_ <- function(x, nm, null.ok = FALSE){
    if(is.null(x)) x <- dpget(nm)
    if(null.ok && is.null(x)){
        "nothing needs doing"
    } else {
        properties(x, nm = nm, class = "logical", length = 1, na.ok = FALSE)
    }
    x
}

dp_positive1_ <- function(x, nm){
    if(is.null(x)) x <- dpget(nm)
    properties(x, nm = nm, class = "numeric", length = 1, na.ok = FALSE)
    if(x < 0){
        s <- paste0("'", nm,"' should be positive")
        stop(s)
    }
    x
}
