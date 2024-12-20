## ------------------------------------------------------------------------ real

##' 'real' comparers
##'
##' functions to compare real variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @name comp-real
NULL

## ------------------------------------------------------------------------ bnry

##' 'bnry' comparers
##'
##' functions to compare bnry variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @name comp-bnry
NULL

## ------------------------------------------------------------------------ catg

##' 'catg' comparers
##'
##' functions to compare catg variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @name comp-catg
NULL

## ----------------------------------------------------------------------- lcat

##' 'lcat' comparers
##'
##' functions to compare lcat variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @name comp-lcat
NULL

## ------------------------------------------------------------------------ date

##' 'date' comparers
##'
##' functions to compare date variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @name comp-date
NULL

## ------------------------------------------------------------------------ surv

##' 'surv' comparers
##'
##' functions to compare surv variables
##' @param time time component of time-to-event variable
##' @param event event component of time-to-event variable
##' @param weight case weight
##' @param g grouping variable
##' @name comp-surv
NULL

##' @rdname comp-surv
##' @details rate_ratio: ratio of the (weighted) rates
##' @export
rate_ratio <- function(time, event, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    e1 <- eventrate(time = time[x_i], event = event[x_i], weight = weight[x_i])
    e2 <- eventrate(time = time[y_i], event = event[y_i], weight = weight[y_i])
    e1$Rate / e2$Rate
}
attr(rate_ratio, "label") <- "Rate Ratio"
