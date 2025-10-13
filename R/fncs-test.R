unusedWeightWarning <- function(..., caller = NULL){
    dots <- list(...)
    x <- dots$weight
    if(!is.null(x)){
        s <- paste0(if(!is.null(caller)) paste0("[", caller,"] ") else NULL,
                    "'weight' not used in this function")
        warning(s)
    }
    invisible(NULL)
}

## tryElseNA <- function(expr){
##     tryCatch(expr = expr, error = function(e){
##         message(e, "\n")
##         NA
##     })
## }

tryElseNA <- function(expr, ..., caller = NULL){
    tryCatch(
        expr = expr,
        error = function(e){
            dots <- list(...)
            term <- dots$.term
            gterm <- dots$.gtab.term
            if(is.null(term)) term <- "<?>"
            caller <- if(!is.null(caller)) paste0("[", caller,"] ") else NULL
            s <- paste0(caller, "testing '", term, "' against '", gterm,
                        "' does not compute; with error:\n")
            message(s, e)
            NA
        },
        warning = function(w){
            dots <- list(...)
            term <- dots$.term
            gterm <- dots$.gtab.term
            if(is.null(term)) term <- "<?>"
            caller <- if(!is.null(caller)) paste0("[", caller,"] ") else NULL
            s <- paste0(caller, "testing '", term, "' again '", gterm,
                        "' is off; with warning:\n")
            message(s, w)
            suppressWarnings(expr)
        }
    )
}


## ------------------------------------------------------------------------ real

##' general testers
##'
##' functions to test any variables
##' @param x input vector
##' @param g grouping variable
##' @param ... arguments passed
##' @name test-all
NULL

META_noTest <- function(bl = TRUE){
    if(bl){
        data.frame(p = NA_real_, p.info = NA_character_)
    } else {
        data.frame(`no test` = NA_real_)
    }
}

##' @rdname test-all
##' @description noTest: no testing
##' @export
noTest <- function(x, g, ...){
    ## unusedWeightWarning(..., caller = "noTest")
    META_noTest(bl = FALSE)
}
## attr(noTest, "label") <- "no test"

##' @rdname test-all
##' @description noTest.bl: no testing (for baseline tables)
##' @export
noTest.bl <- function(x, g, ...){
    ## unusedWeightWarning(..., caller = "noTest.bl")
    META_noTest(bl = TRUE)
}
attr(noTest.bl, "meta") <- c("p.info")

## ------------------------------------------------------------------------ real

##' 'real' testers
##'
##' functions to test real variables
##' @param x input vector
##' @param g grouping variable
##' @param ... arguments passed
##' @importFrom stats wilcox.test kruskal.test anova lm
##' @name test-real
NULL

META_param <- function(x, g, bl = TRUE, ...){
    g <- factor(g)
    n_lev <- length(levels(g))
    p <- NA_real_
    p.info <- if(n_lev<=1) "no test" else if(n_lev==2) "t-test" else "F-test"
    if(n_lev > 1){
        ## p <- tryElseNA(anova(lm(x ~ g, weights = weight))[['Pr(>F)']][1])
        p <- tryElseNA(stats::anova(stats::lm(x ~ g))[['Pr(>F)']][1], ...,
                       caller = "META_param")
    }
    if(bl){
       data.frame(p = p, p.info = p.info)
    } else{
        r <- data.frame(p = p)
        names(r) <- p.info
        r
    }
}

META_nonparam <- function(x, g, bl = TRUE, ...){
    g <- factor(g)
    n_lev <- length(levels(g))
    p <- NA_real_
    p.info <- "no test"
    if(n_lev == 2){
        p <- tryElseNA(stats::wilcox.test(x~g)$p.value, ...,
                       caller = "META_nonparam")
        p.info <- "Wilcoxon" ## "Wilcoxon-Mann-Whitney"
    }
    if(n_lev > 2){
        p <- tryElseNA(stats::kruskal.test(x~g)$p.value, ...,
                       caller = "META_nonparam")
        p.info <- "Kruskal-Wallis"
    }
    if(bl){
        data.frame(p = p, p.info = p.info)
    } else {
        r <- data.frame(p = p)
        names(r) <- p.info
        r
    }
}

##' @rdname test-real
##' @description param: parametric test via linear model
##' @export
param <- function(x, g, ...){
    unusedWeightWarning(..., caller = "param")
    META_param(x = x, g = g, bl = FALSE, ...)
}
## attr(param, "part") <- "test"

##' @rdname test-real
##' @description param.bl: parametric test via linear model (for baseline tables)
##' @export
param.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "param.bl")
    META_param(x = x, g = g, bl = TRUE, ...)
}
attr(param.bl, "meta") <- c("p.info")

##' @rdname test-real
##' @description nonparam: non-parametric test
##' @export
nonparam <- function(x, g, ...){
    unusedWeightWarning(..., caller = "nonparam")
    META_nonparam(x = x, g = g, bl = FALSE, ...)
}
## attr(nonparam, "label") <- "p"

##' @rdname test-real
##' @description nonparam.bl: non-parametric test (for baseline tables)
##' @export
nonparam.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "nonparam.bl")
    META_nonparam(x = x, g = g, bl = TRUE, ...)
}
attr(nonparam.bl, "meta") <- c("p.info")

## ------------------------------------------------------------------------ catg

##' 'catg' testers
##'
##' functions to test catg variables
##' @param x input vector
##' @param g grouping variable
##' @param ... arguments passed
##' @importFrom stats chisq.test
##' @name test-catg
NULL

META_chisq <- function(x, g, bl = TRUE, ...){
    g <- factor(g)
    n_lev <- length(levels(g))
    p <- NA_real_
    p.info <- "no test"
    if(n_lev > 1){
        ## browser()
        p <- tryElseNA(stats::chisq.test(x,g)$p.value, ...,
                       caller = "META_chisq")
        p.info <- "Chi-square"
    }
    if(bl){
        data.frame(p = p, p.info = p.info)
    } else {
        r <- data.frame(p = p)
        names(r) <- "p"
        r
    }
}

##' @rdname test-catg
##' @description catg.chisq:  chisquare-test for categorical variables
##' @export
catg.chisq <- function(x, g, ...){
    unusedWeightWarning(..., caller = "catg.chisq")
    META_chisq(x = x, g = g, bl = FALSE, ...)
}

##' @rdname test-catg
##' @description catg.chisq.bl: chisquare-test for categorical variables (baseline)
##' @export
catg.chisq.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "catg.chisq.bl")
    META_chisq(x = x, g = g, bl = TRUE, ...)
}
attr(catg.chisq.bl, "meta") <- c("p.info")


## ------------------------------------------------------------------------ date

##' 'date' testers
##'
##' functions to test date variables
##' @param x input vector
##' @param g grouping variable
##' @param ... arguments passed
##' @name test-date
NULL

##' @rdname test-date
##' @description date.nonparam: non-parametric test of dates treated as integers
##' @export
date.nonparam <- function(x, g, ...){
    unusedWeightWarning(..., caller = "date.nonparam")
    META_nonparam(x = as.integer(x), g = g, bl = FALSE, ...)
}
##attr(nonparam, "label") <- "p"

##' @rdname test-date
##' @description date.nonparam.bl: non-parametric test of dates treated as
##'     integers (for baseline tables)
##' @export
date.nonparam.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "date.nonparam.bl")
    META_nonparam(x = as.integer(x), g = g, bl = TRUE, ...)
}
attr(nonparam, "meta") <- c("p.info")

## ------------------------------------------------------------------------ surv

##' 'surv' testers
##'
##' functions to test surv variables
##' @param time time component of time-to-event variable
##' @param event event component of time-to-event variable
##' @param g grouping variable
##' @param ... arguments passed
##' @importFrom survival survdiff Surv
##' @name test-surv
NULL

##' @rdname test-surv
##' @description logrank: logrank test of survival curves
##' @export
logrank <- function(time, event, g, ...){
    unusedWeightWarning(..., caller = "logrank")
    tryElseNA(survival::survdiff(survival::Surv(time, event) ~ g)$pvalue, ...,
              caller = "logrank")
}
attr(logrank, "label") <- "Logrank"

##' @rdname test-surv
##' @description logrank.bl: logrank test of survival curves (for baseline tables)
##' @export
logrank.bl <- function(time, event, g, ...){
    unusedWeightWarning(..., caller = "logrank.bl")
    data.frame(p = logrank(time, event, g, ...),
               p.info = "Log rank")
}
attr(logrank.bl, "meta") <- c("p.info")
