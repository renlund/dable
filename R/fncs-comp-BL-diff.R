##' default baseline comparer 'diff'
##'
##' Comparer functions to calculate differences that work together.
##' @param x input vector
##' @param g grouping variable
##' @param weight case weight
##' @param ... arguments passed
##' @name baseline-compare-diff
NULL

##' @rdname baseline-compare-diff
##' @description real.diff.bl: (weighted) 'real' standardized difference
##' @export
real.diff.bl <- function(x, g, weight = NULL, ...){
    data.frame(Variable = di.Variable(x, ...),
               comp = real.diff(x = x, g = g, weight = weight),
               comp.info = .diff())
}
attr(real.diff.bl, "meta") <- c("Variable", "comp.info")

##' @rdname baseline-compare-diff
##' @description catg.diff.bl: (weighted) 'catg' standardized difference
##' @export
catg.diff.bl <- function(x, g, weight = NULL, ...){
    d <- catg.diff(x = x, g = g, weight = weight, ...)
    names(d)[2] <- "comp"
    d$comp.info = .diff()
    d
}
attr(catg.diff.bl, "meta") <- c("Variable", "comp.info")

##' @rdname baseline-compare-diff
##' @description bnry.diff.bl: (weighted) standardized difference for 'bnry'
##' @export
bnry.diff.bl <- function(x, g, weight = NULL, ...){
    data.frame(Variable = di.Variable(x, ...),
               comp = bnry.diff(x = x, g = g, weight = weight),
               comp.info = .diff())
}
attr(bnry.diff.bl, "meta") <- c("Variable", "comp.info")

##' @rdname baseline-compare-diff
##' @description lcat.diff.bl: empty
##' @export
lcat.diff.bl <- function(x, g, weight = NULL, ...){
    data.frame(Variable = di.Variable(x, ...),
               comp = NA_real_,
               comp.info = NA_character_) ## .diff() ?
}
attr(lcat.diff.bl, "meta") <- c("Variable", "comp.info")

##' @rdname baseline-compare-diff
##' @description date.diff.bl: (weighted) standardized difference for 'date' ( = real.diff
##'     applied to x interpreted as an integer)
##' @export
date.diff.bl <- function(x, g, weight = NULL, ...){
    data.frame(Variable = di.Variable(x, ...),
               comp = date.diff(x = x, g = g, weight = weight),
               comp.info = .diff())
}
attr(date.diff.bl, "meta") <- c("Variable", "comp.info")

##' @rdname baseline-compare-diff
##' @description surv.diff.bl: (weighted) standardized difference for 'surv'
##' @export
surv.diff.bl <- function(time, event, g, weight = NULL, ...){
    data.frame(Variable = di.Variable(x, ...),
               comp = surv.diff(time = time, event = event,
                               g = g, weight = weight),
               comp.info = .diff())
}
attr(surv.diff.bl, "meta") <- c("Variable", "comp.info")
