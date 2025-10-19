##' default baseline comparer 'std'
##'
##' Comparer functions to calculate standardized differences that work together.
##' @param x input vector
##' @param g grouping variable
##' @param weight case weight
##' @param ... arguments passed
##' @name baseline-compare-std
NULL

##' @rdname baseline-compare-std
##' @description real.std.bl: (weighted) 'real' standardized difference
##' @export
real.std.bl <- function(x, g, weight = NULL, ...){
    data.frame(comp = real.std(x = x, g = g, weight = weight),
               comp.info = .stddiff())
}
attr(real.std.bl, "meta") <- "comp.info"

##' @rdname baseline-compare-std
##' @description catg.std.bl: (weighted) 'catg' standardized difference
##' @export
catg.std.bl <- function(x, g, weight = NULL, ...){
    data.frame(comp = catg.std(x = x, g = g, weight = weight),
               comp.info = .stddiff())
}
attr(catg.std.bl, "meta") <- "comp.info"

##' @rdname baseline-compare-std
##' @description bnry.std.bl: (weighted) standardized difference for 'bnry'
##' @export
bnry.std.bl <- function(x, g, weight = NULL, ...){
    data.frame(comp = bnry.std(x = x, g = g, weight = weight),
               comp.info = .stddiff())
}
attr(bnry.std.bl, "meta") <- "comp.info"

##' @rdname baseline-compare-std
##' @description lcat.std.bl: empty
##' @export
lcat.std.bl <- function(x, g, weight = NULL, ...){
    data.frame(comp = NA_real_,
               comp.info = NA_character_) ## .stddiff() ?
}
attr(lcat.std.bl, "meta") <- "comp.info"

##' @rdname baseline-compare-std
##' @description date.std.bl: (weighted) standardized difference for 'date' ( = real.std
##'     applied to x interpreted as an integer)
##' @export
date.std.bl <- function(x, g, weight = NULL, ...){
    data.frame(comp = date.std(x = x, g = g, weight = weight),
               comp.info = .stddiff())
}
attr(date.std.bl, "meta") <- "comp.info"

##' @rdname baseline-compare-std
##' @description surv.std.bl: (weighted) standardized difference for 'surv'
##' @export
surv.std.bl <- function(time, event, g, weight = NULL, ...){
    data.frame(comp = surv.std(time = time, event = event,
                               g = g, weight = weight),
               comp.info = .stddiff())
}
attr(surv.std.bl, "meta") <- "comp.info"
