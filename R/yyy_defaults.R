##' dable parameters and how to set and get them
##'
##' @details These are functions to set and retrieve dable parameters. The
##'     parameters in the package are:
##'
##' * *bnry.list* the list of values defining the bnry type
##'
##' * *real.tol* the threshold for number of unique numeric values to be
##' considered to be type catg
##'
##' * *catg.tol* the threshold for number of unique categorical values to be
##' considered to be type lcat
##'
##' * *surv.prefix* are surv variables identifiable through a prefix? (else suffix)
##'
##' * *surv.affix* what is the affix? default: c("time" = "t.", "event" = "ev.")
##'
##' * *id* character vector of id variable names, default: NULL
##'
##' * *gtab.defvar.rm* if gtab is specified by a term, should that term be hidden
##' in output?
##'
##' * *weight.defvar.rm* if weight is specified by a term (must be, currently),
##' should that term be hidden in output?
##'
##' * *unit.id.rm* should the unit (first element of 'id') id be hidden in output?
##'
##' * *vtab.group.name* default grouping name (for all but surv type) if not given
##'
##' * *stab.group.name* default grouping name for surv type if not given
##'
##' * *gtab.group.name* default gtab 'all' name if not given
##'
##' * *comp.header* header for comparison column i baseline tables
##'
##' * *test.header* header for test column i baseline tables
##'
##' * *units.name* how to refer to the units in text, default: subjects
##'
##' * *grey.first* if using alternating grey in output table, begin with a grey line?
##'
##' * *digits* number of digits presented after formatting? Vector of length
##' three to give number of digits for small (absolute value smaller than 1),
##' medium (absolute value between 1 and 10) and large (absolute value larger
##' than 10) numbers. Default: c("small" = 2, "mid" = 2, "large" = 1)
##'
##' * *scientific* if to use scientific number representation, and what the cut
##' offs should be, default: list(use = TRUE, low = 1e-8, high = 1e8)
##'
##' * *p* for formatting, are numbers p-value? default: FALSE (but this is
##' generally automatically identified by formatting functions)
##'
##' * *p.bound* cut off for p-value presentation
##'
##' * *NAtext* how to show NA in tables
##'
##' * *NAalias* how to refer to missing
##'
##' * *sep* how to separate the concatenation of variable name and its level
##' (when applicable)
##'
##' * *output* the type of output; mainly used for formatting
##'
##' * *indent* what string to use as indentation, default "\\t ". Note: flextable
##' output will automatically substitute "\\t" for a format specific
##' alternative. The formatting functions in this package will substitute "\\t"
##' with "\\quad". Thus, one does not in general need to alter this parameter.
##'
##' * *percent* (NOT IN USE) symbol to use for percent.
##'
##' * *date.format* the format used for dates, default "%y%m%d"
##'
##' * *substitute* data.frame of substitutions for formatting. (XK add details)
##'
##' * *type.part* where type is any type (real,catg,...) and part is the table
##' part (desc, comp, test); these set the default such function for simple
##' descriptive tables
##'
##' * *type.part.bl* where type is any type (real,catg,...) and part is the table
##' part (desc, comp, test); these set the default such function for baseline tables
##'
##' @name dable-parameters
NULL

dable_parameters <- list(
    dable.bnry.list = list(c(0, 1),
                            c(FALSE, TRUE),
                            c("No", "Yes"),
                            c("no", "yes")),
    dable.real.tol = 5,
    dable.catg.tol = 50,
    dable.surv.prefix = TRUE,
    dable.surv.affix = c("time" = "t.", "event" = "ev."),
    dable.id = NULL,
    dable.gtab.defvar.rm = TRUE,
    dable.weight.defvar.rm = TRUE,
    dable.unit.id.rm = TRUE,
    dable.vtab.group.name = "Covariates",
    dable.stab.group.name = "Time-to-event",
    dable.gtab.group.name = "All",
    dable.comp.header = "Compare",
    dable.test.header = "Test",
    ## printingish
    dable.units.name = "subjects",
    dable.grey.first = FALSE,
    ## formatting
    dable.digits = c("small" = 2, "mid" = 2, "large" = 1),
    dable.scientific = list(use = TRUE, low = 1e-8, high = 1e8),
    dable.p = FALSE,
    dable.p.bound = 1e-4,
    dable.NAtext = "",
    dable.NAalias = "Missing",
    dable.sep = ": ",
    dable.output = "latex",
    dable.indent = "\t ",
    dable.percent = "%",     ## NOT IN USE (as of 2025-10)
    dable.date.format = "%y%m%d",
    dable.substitute = data.frame(
        out = c("latex",
                "latex",
                "console"),
        pat = c("(.*)([^\\\\])(%)(.*)",
                "\t",
                "\t"),
        rep = c("\\1\\2\\\\%\\4",
                "\\quad ",
                "   "),
        fix = c(FALSE,
                TRUE,
                TRUE)
    ),
    ## default describers --------------------
    dable.real.desc = "mean_sd",
    dable.catg.desc = "catg.count_prop",
    dable.bnry.desc = "bnry.count_prop",
    dable.lcat.desc = "n.unique",
    dable.date.desc = "date.min_max",
    dable.surv.desc = "eventrate",
    ## default comparers
    dable.real.comp = "real.std",
    dable.catg.comp = "catg.std",
    dable.bnry.comp = NULL,
    dable.lcat.comp = "empty.std",
    dable.date.comp = "date.std",
    dable.surv.comp = "surv.std",
    ## default testers
    dable.real.test = "param",
    dable.catg.test = "catg.chisq",
    dable.bnry.test = NULL,
    dable.lcat.test = "noTest",
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
    dable.real.comp.bl = "real.std.bl",
    dable.catg.comp.bl = "catg.std.bl",
    dable.bnry.comp.bl = NULL,
    dable.lcat.comp.bl = "empty.std.bl",
    dable.date.comp.bl = "date.std.bl",
    dable.surv.comp.bl = "surv.std.bl",
    ## default baseline tester
    dable.real.test.bl = "param.bl",
    dable.catg.test.bl = "catg.chisq.bl",
    dable.bnry.test.bl = NULL,
    dable.lcat.test.bl = "noTest.bl",
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


##' @rdname dable-parameters
##' @details dpset: set a dable package parameter
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

##' @rdname dable-parameters
##' @details dpset_defaults: set default parameter values
##' @param overwrite logical; overwrite parameter values already set?
##' @export
dpset_defaults <- function(overwrite = TRUE, style = NA_character_){
    properties(style, class = "character", length = 1, na.ok = TRUE)
    if(overwrite){
        options(dable_parameters)
    } else {
        op <- options()
        toset <- !(names(dable_parameters) %in% names(op))
        if(any(toset)) options(dable_parameters[toset])
    }
    if(!is.na(style)){
        one_of(style, set = c("latex", "flextable", "console"))
        dpset("output", style)
        if(style %in% "console"){
            dpset("indent", value = "    ")
        }
    }
    invisible()
}


subtypes.key <- stats::setNames(object = c("catg", "catg"),
                                nm = c("bnry", "lcat"))

##' @rdname dable-parameters
##' @details dpget: get package parameter
##' @param verbose logical; get (possibly) helpful messages?
##' @param sub logical; search for possible replacement values?
##' @importFrom stats setNames
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

##' @rdname dable-parameters
##' @details dpget_all: get all package parameters
##' @export
dpget_all <- function(){
    z <- names(options())
    options()[grep(pattern = "^dable\\.", x = z, value = TRUE)]
}

##' @rdname dable-parameters
##' @details add2bnry.list: add a value to the 'bnry.list'
##' @param bl list of binary values
##' @export
add2bnry.list <- function(bl){
    add <- dp_bnry.list(bl)
    dpset(param = "bnry.list", value = c(dpget("bnry.list"), add))
}

##' @rdname dable-parameters
##' @details add2substitute: add to the substitute data.frame
##' @param out character; subtitution for which output?
##' @param pat character; later passed to gsub arg 'pattern'
##' @param rep character; later passed to gsub arg 'replacement'
##' @param fix logical; later passed to gsub arg 'fixed'
##' @export
add2substitute <- function(out, pat, rep, fix){
    properties(out, class = "character", na.ok = FALSE)
    properties(pat, class = "character", na.ok = FALSE)
    properties(rep, class = "character", na.ok = FALSE)
    properties(fix, class = "logical", na.ok = FALSE)
    r <- dpget("substitute")
    R <- rbind(r, data.frame(out=out, pat=pat, rep=rep, fix=fix))
    dpset("substitute", R)
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
        surv.prefix = dp_logic1_(value, p),
        surv.affix = dp_surv.affix(value),
        id = dp_id(value),
        gtab.defvar.rm = dp_logic1_(value, p),
        weight.defvar.rm = dp_logic1_(value, p),
        unit.id.rm = dp_logic1_(value, p),
        stab.group.name = dp_char1_(value, p),
        vtab.group.name = dp_char1_(value, p),
        gtab.group.name = dp_char1_(value, p),
        comp.header = dp_char1_(value, p),
        test.header = dp_char1_(value, p),
        units.name = dp_char1_(value, p),
        digits = dp_digits(value),
        scientific = dp_scientific(value),
        p = dp_logic1_(value, p),
        p.bound = dp_positive1_(value, p),
        NAtext = dp_char1_(value, p),
        NAalias = dp_char1_(value, p),
        grey.first = dp_logic1_(value, p),
        output = dp_char1_(value, p),
        sep = dp_char1_(value, p),
        indent = dp_indent(value),
        percent = dp_char1_(value, p),
        date.format = dp_char1_(value, p),
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

dp_scientific <- function(x = NULL){
    if(is.null(x)) x <- dpget("scientific")
    properties(x, class = "list", length = 3)
    req_nm <- c("use", "low", "high")
    if(!all(req_nm %in% names(x))){
        s <- paste0("'scientific' must be a list with elements: ",
                    paste0(req_nm, collapse = ", "))
        stop(s)
    }
    dp_logic1_(x$use, "use")
    dp_positive1_(x$low, "low")
    dp_positive1_(x$high, "high")
    x
}

dp_id <- function(x = NULL){
    if(is.null(x)) x <- dpget("id")
    if(!is.null(x)) properties(x, class = "character", na.ok = FALSE)
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
    if(is.null(x)) x <- dpget("indent")
    dp_char1_(x = x, nm = "indent")
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
