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

tryElseNA <- function(expr){
    tryCatch(expr = expr, error = function(e){
        message(e, "\n")
        NA
    })
}

## ------------------------------------------------------------------------ real

##' general testers
##'
##' functions to test any variables
##' @param x input vector
##' @param g grouping variable
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
    META_noTest(x = x, g = g, bl = FALSE)
}
## attr(noTest, "part") <- "test"

##' @rdname test-all
##' @description noTest.bl: no testing (for baseline tables)
##' @export
noTest.bl <- function(x, g, ...){
    ## unusedWeightWarning(..., caller = "noTest.bl")
    META_noTest(x = x, g = g, bl = TRUE)
}
## attr(noTest.bl, "part") <- c("test", "meta")

## ------------------------------------------------------------------------ real

##' 'real' testers
##'
##' functions to test real variables
##' @param x input vector
##' @param g grouping variable
##' @name test-real
NULL

META_param <- function(x, g, bl = TRUE){
    g <- factor(g)
    n_lev <- length(levels(g))
    p <- NA_real_
    p.info <- if(n_lev<=1) "no test" else if(n_lev==2) "t-test" else "F-test"
    if(n_lev > 1){
        ## p <- anova(lm(x ~ g))[['Pr(>F)']][1]
        p <- tryElseNA(anova(lm(x ~ g))[['Pr(>F)']][1])
    }
    if(bl){
       data.frame(p = p, p.info = p.info)
    } else{
        r <- data.frame(p = p)
        names(r) <- p.info
        r
    }
}

META_nonparam <- function(x, g, bl = TRUE){
    g <- factor(g)
    n_lev <- length(levels(g))
    p <- NA_real_
    p.info <- "no test"
    if(n_lev == 2){
        ## p <- stats::wilcox.test(x~g)$p.value
        p <- tryElseNA(stats::wilcox.test(x~g)$p.value)
        p.info <- "Wilcoxon" ## "Wilcoxon-Mann-Whitney"
    }
    if(n_lev > 2){
        ## p <- stats::kruskal.test(x~g)$p.value
        p <- tryElseNA(stats::kruskal.test(x~g)$p.value)
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
    META_param(x = x, g = g, bl = FALSE)
}
## attr(param, "part") <- "test"

##' @rdname test-real
##' @description param.bl: parametric test via linear model (for baseline tables)
##' @export
param.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "param.bl")
    META_param(x = x, g = g, bl = TRUE)
}
##attr(param.bl, "part") <- c("test", "meta")

##' @rdname test-real
##' @description nonparam: non-parametric test
##' @export
nonparam <- function(x, g, ...){
    unusedWeightWarning(..., caller = "nonparam")
    META_nonparam(x = x, g = g, bl = FALSE)
}
##attr(nonparam, "part") <- "test"

##' @rdname test-real
##' @description nonparam.bl: non-parametric test (for baseline tables)
##' @export
nonparam.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "nonparam.bl")
    META_nonparam(x = x, g = g, bl = TRUE)
}
##attr(nonparam.bl, "part") <- c("test", "meta")

## ------------------------------------------------------------------------ catg

##' 'catg' testers
##'
##' functions to test catg variables
##' @param x input vector
##' @param g grouping variable
##' @name test-catg
NULL

META_chisq <- function(x, g, bl = TRUE, catg.full.length = TRUE){
    g <- factor(g)
    n_lev <- length(levels(g))
    p <- NA_real_
    p.info <- "no test"
    if(n_lev > 1){
        ## browser()
        p <- tryElseNA(stats::chisq.test(x,g)$p.value)
        p.info <- "Chi-square"
    }
    if(catg.full.length){
        k <- length(levels(as.factor(x)))
        p <- c(p, rep(NA, max(k-1,0)))
        if(bl) p.info <- c(p.info, rep(NA, max(k-1,0)))
    }
    if(bl){
        data.frame(p = p, p.info = p.info)
    } else {
        r <- data.frame(p = p)
        names(r) <- p.info
        r
    }
}

##' @rdname test-catg
##' @description catg.chisq:  chisquare-test for categorical variables
##' @export
catg.chisq <- function(x, g, ...){
    unusedWeightWarning(..., caller = "catg.chisq")
    META_chisq(x = x, g = g, bl = FALSE, catg.full.length = TRUE)
}
##attr(catg.chisq, "part") <- "test"

##' @rdname test-catg
##' @description catg.chisq.bl: chisquare-test for categorical variables (baseline)
##' @export
catg.chisq.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "catg.chisq.bl")
    META_chisq(x = x, g = g, bl = TRUE, catg.full.length = TRUE)
}
##attr(catg.chisq.bl, "part") <- c("test", "p.info")

## ------------------------------------------------------------------------ bnry

##' 'bnry' testers
##'
##' functions to test bnry variables
##' @param x input vector
##' @param g grouping variable
##' @name test-bnry
NULL

##' @rdname test-bnry
##' @description bnry.chisq: chisquare-test for binary variables
##' @export
bnry.chisq <- function(x, g, ...){
    unusedWeightWarning(..., caller = "bnry.chisq")
    META_chisq(x = x, g = g, bl = FALSE, catg.full.length = FALSE)
}
##attr(bnry.chisq, "part") <- "test"

##' @rdname test-bnry
##' @description bnry.chisq.bl: chisquare-test for binary variables (baseline)
##' @export
bnry.chisq.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "bnry.chisq.bl")
    META_chisq(x = x, g = g, bl = TRUE, catg.full.length = FALSE)
}
##attr(bnry.chisq.bl, "part") <- c("test", "p.info")

## ----------------------------------------------------------------------- lcat

##' 'lcat' testers
##'
##' functions to test lcat variables
##' @param x input vector
##' @param g grouping variable
##' @name test-lcat
NULL

## ------------------------------------------------------------------------ date

##' 'date' testers
##'
##' functions to test date variables
##' @param x input vector
##' @param g grouping variable
##' @name test-date
NULL

##' @rdname test-date
##' @description date.nonparam: non-parametric test of dates treated as integers
##' @export
date.nonparam <- function(x, g, ...){
    unusedWeightWarning(..., caller = "date.nonparam")
    META_nonparam(x = as.integer(x), g = g, bl = FALSE)
}
##attr(nonparam, "part") <- "test"

##' @rdname test-date
##' @description date.nonparam.bl: non-parametric test of dates treated as
##'     integers (for baseline tables)
##' @export
date.nonparam.bl <- function(x, g, ...){
    unusedWeightWarning(..., caller = "date.nonparam.bl")
    META_nonparam(x = as.integer(x), g = g, bl = TRUE)
}
##attr(nonparam, "part") <- c("test", "meta")

## ------------------------------------------------------------------------ surv

##' 'surv' testers
##'
##' functions to test surv variables
##' @param time time component of time-to-event variable
##' @param event event component of time-to-event variable
##' @param g grouping variable
##' @name test-surv
NULL

##' @rdname test-surv
##' @description logrank: logrank test of survival curves
##' @export
logrank <- function(time, event, g, ...){
    unusedWeightWarning(..., caller = "logrank")
    tryElseNA(survival::survdiff(survival::Surv(time, event) ~ g)$pvalue)
}
attr(logrank, "label") <- "Logrank"

##' @rdname test-surv
##' @description logrank.bl: logrank test of survival curves (for baseline tables)
##' @export
logrank.bl <- function(time, event, g, ...){
    unusedWeightWarning(..., caller = "logrank.bl")
    data.frame(p = logrank(time, event, g),
               p.info = "Log rank")
}
##attr(logrank.bl, "part") <- c("test", "meta")
